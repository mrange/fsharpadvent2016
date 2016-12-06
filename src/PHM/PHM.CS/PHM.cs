// ----------------------------------------------------------------------------------------------
// Copyright 2016 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------

// Inspired by Clojure's Persistent Hash Map (https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/PersistentHashMap.java)
//  and Phil Bagwell's Ideal Hash Trie (http://lampwww.epfl.ch/papers/idealhashtrees.pdf)
//  and http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel

namespace PHM.CS
{
  using System;
  using System.Collections;
  using System.Collections.Generic;
  using System.Diagnostics;
  using System.Globalization;
  using System.Runtime.CompilerServices;
  using System.Text;

  abstract partial class PersistentHashMap<K, V> : IEnumerable<KeyValuePair<K ,V>>
    where K : IEquatable<K>
  {
    internal static readonly PersistentHashMap.EmptyNode<K, V> EmptyNode = new PersistentHashMap.EmptyNode<K, V> ();

    public bool IsEmpty
    {
      [MethodImpl (MethodImplOptions.AggressiveInlining)]
      get
      {
        return Empty ();
      }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public bool Visit (Func<K, V, bool> r)
    {
      if (r != null)
      {
        return Receive (r);
      }
      else
      {
        return true;
      }
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public PersistentHashMap<K, V> Set (K k, V v)
    {
      var h = (uint)k.GetHashCode ();
      return Set (h, 0, new PersistentHashMap.KeyValueNode<K, V> (h, k, v));
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public bool TryFind (K k, out V v)
    {
      return TryFind ((uint)k.GetHashCode (), 0, k, out v);
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    public PersistentHashMap<K, V> Unset (K k)
    {
      return Unset ((uint)k.GetHashCode (), 0, k) ?? EmptyNode;
    }

#if PHM_TEST_BUILD
    public bool CheckInvariant ()
    {
      return CheckInvariant (0, 0);
    }
#endif

    public abstract IEnumerator<KeyValuePair<K, V>> GetEnumerator();

    IEnumerator IEnumerable.GetEnumerator()
    {
      return GetEnumerator ();
    }

#if PHM_TEST_BUILD
    public override string ToString ()
    {
      var sb = new StringBuilder (16);
      Describe (sb, 0);
      return sb.ToString ();
    }
#endif

#if PHM_TEST_BUILD
    internal abstract bool                      CheckInvariant  (uint h, int s);
    internal abstract void                      Describe        (StringBuilder sb, int indent);
#endif
    internal virtual  bool                      Empty           ()
    {
      return false;
    }
    internal abstract bool                      Receive         (Func<K, V, bool> r);
    internal abstract PersistentHashMap<K, V>  Set             (uint h, int s, PersistentHashMap.KeyValueNode<K, V> n);
    internal abstract bool                      TryFind         (uint h, int s, K k, out V v);
    internal abstract PersistentHashMap<K, V>  Unset           (uint h, int s, K k);
  }

  static partial class PersistentHashMap
  {
    public static PersistentHashMap<K, V> Empty<K, V> ()
      where K : IEquatable<K>
    {
      return PersistentHashMap<K ,V>.EmptyNode;
    }

    internal const int TrieShift    = 4                 ;
    internal const int TrieMaxShift = 32                ;
    internal const int TrieMaxNodes = 1 << TrieShift    ;
    internal const int TrieMask     = TrieMaxNodes - 1  ;

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal static uint LocalHash (uint h, int s)
    {
      return (h >> s) & TrieMask;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal static uint Bit (uint h, int s)
    {
      return 1U << (int) LocalHash (h, s);
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal static int PopCount (uint v)
    {
      // TODO: As we only need popcount for 16bits can this be optimized?
      // From: http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
      //  many cpus support popcount natively but that isn't available in IL
      v -=  ((v >> 1) & 0x55555555);
      v =   (v & 0x33333333) + ((v >> 2) & 0x33333333);
      v =   ((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24;
      return (int)v;
    }

    internal static string FormatWith (this string format, params object[] args)
    {
      return string.Format (CultureInfo.InvariantCulture, format, args);
    }

    internal static StringBuilder IndentedLine (this StringBuilder sb, int indent, string line)
    {
      sb.Append (' ', indent);
      sb.AppendLine (line);
      return sb;
    }

    internal static StringBuilder FormatIndentedLine (this StringBuilder sb, int indent, string format, params object[] args)
    {
      return sb.IndentedLine (indent, format.FormatWith (args));
    }

    internal static bool CheckHash (uint h, uint ah, int s)
    {
      return (h & ((1 << s) - 1)) == ah;
    }

    // Note: Array.Copy seems significantly faster than for loops

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal static T[] CopyArray<T> (T[] vs)
    {
      var nvs = new T[vs.Length];
      Array.Copy (vs, nvs, vs.Length);
      return nvs;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal static T[] CopyArrayMakeHoleLast<T> (T[] vs, T hole)
    {
      var nvs = new T[vs.Length + 1];
      Array.Copy (vs, nvs, vs.Length);
      nvs[vs.Length] = hole;
      return nvs;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal static T[] CopyArrayMakeHole<T> (int at, T[] vs, T hole)
    {
      Debug.Assert (at <= vs.Length);

      var nvs = new T[vs.Length + 1];
      Array.Copy (vs, nvs, at);
      Array.Copy (vs, at, nvs, at + 1, vs.Length - at);
      nvs[at] = hole;
      return nvs;
    }

    [MethodImpl (MethodImplOptions.AggressiveInlining)]
    internal static T[] CopyArrayRemoveHole<T> (int at, T[] vs)
    {
      Debug.Assert (at < vs.Length);
      Debug.Assert (vs.Length > 1);

      var nvs = new T[vs.Length - 1];
      Array.Copy (vs, nvs, at);
      Array.Copy (vs, at + 1, nvs, at, vs.Length - at - 1);
      return nvs;
    }

    internal sealed partial class EmptyNode<K, V> : PersistentHashMap<K, V>
      where K : IEquatable<K>
    {
      public override IEnumerator<KeyValuePair<K, V>> GetEnumerator()
      {
        yield break;
      }

#if PHM_TEST_BUILD
      internal override bool CheckInvariant (uint h, int s)
      {
        return true;
      }

      internal override void Describe (StringBuilder sb, int indent)
      {
        sb.IndentedLine (indent, "Empty");
      }
#endif
      internal override bool Empty ()
      {
        return true;
      }

      internal override bool Receive (Func<K, V, bool> r)
      {
        return true;
      }

      internal sealed override PersistentHashMap<K, V> Set (uint h, int s, KeyValueNode<K, V> n)
      {
        return n;
      }

      internal sealed override bool TryFind (uint h, int s, K k, out V v)
      {
        v = default (V);
        return false;
      }

      internal override PersistentHashMap<K, V> Unset (uint h, int s, K k)
      {
        return null;
      }
    }

    internal sealed partial class KeyValueNode<K, V> : PersistentHashMap<K, V>
      where K : IEquatable<K>
    {
      public readonly uint Hash  ;
      public readonly K    Key   ;
      public readonly V    Value ;

      [MethodImpl (MethodImplOptions.AggressiveInlining)]
      public KeyValueNode (uint h, K k, V v)
      {
        Hash  = h;
        Key   = k;
        Value = v;
      }

      public override IEnumerator<KeyValuePair<K, V>> GetEnumerator()
      {
        yield return new KeyValuePair<K, V> (Key, Value);
      }

#if PHM_TEST_BUILD
      internal override bool CheckInvariant (uint h, int s)
      {
        return CheckHash (Hash, h, s) && (Hash == (uint)Key.GetHashCode ());
      }

      internal override void Describe (StringBuilder sb, int indent)
      {
        sb.FormatIndentedLine (indent, "KeyValue Hash:0x{0:X08}, Key:{1}, Value:{2}", Hash, Key, Value);
      }
#endif

      internal override bool Receive (Func<K, V, bool> r)
      {
        return r (Key, Value);
      }

      internal sealed override PersistentHashMap<K, V> Set (uint h, int s, KeyValueNode<K, V> n)
      {
        // TODO: Optimize if h,k and v are identical?

        // No need to check for reference equality as parent always creates new KeyValueNode
        if (Hash == h && Key.Equals (n.Key))
        {
          // Replaces current node
          return n;
        }
        else if (Hash == h)
        {
          return HashCollisionNodeN<K ,V>.FromTwoNodes (h, this, n);
        }
        else
        {
          return BitmapNodeN<K ,V>.FromTwoNodes (s, Hash, this, h, n);
        }
      }

      internal sealed override bool TryFind (uint h, int s, K k, out V v)
      {
        if (Hash == h && Key.Equals (k))
        {
          v = Value;
          return true;
        }
        else
        {
          v = default (V);
          return false;
        }
      }

      internal override PersistentHashMap<K, V> Unset (uint h, int s, K k)
      {
        if (Hash == h && Key.Equals (k))
        {
          return null;
        }
        else
        {
          return this;
        }
      }
    }

    internal sealed partial class BitmapNode1<K, V> : PersistentHashMap<K, V>
      where K : IEquatable<K>
    {
      public readonly uint                      Bitmap  ;
      public readonly PersistentHashMap<K, V>  Node    ;

      [MethodImpl (MethodImplOptions.AggressiveInlining)]
      public BitmapNode1 (uint b, PersistentHashMap<K, V> n)
      {
        Bitmap  = b ;
        Node    = n ;
      }

      public override IEnumerator<KeyValuePair<K, V>> GetEnumerator()
      {
        foreach (var kv in Node)
        {
          yield return kv;
        }
      }

#if PHM_TEST_BUILD
      internal override bool CheckInvariant (uint h, int s)
      {
        if (PopCount (Bitmap) != 1)
        {
          return false;
        }

        var bitmap = Bitmap;

        var localIdx = PopCount (Bitmap - 1);

        if (!Node.CheckInvariant (h | (uint)(localIdx << s), s + TrieShift))
        {
          return false;
        }

        return true;
      }

      internal override void Describe (StringBuilder sb, int indent)
      {
        var bits = Convert.ToString (Bitmap, 2);
        sb.FormatIndentedLine (indent, "Bitmap1 Bitmap:0b{0}", new string ('0', 16 - bits.Length) + bits);
        Node.Describe (sb, indent + 2);
      }
#endif

      internal override bool Receive (Func<K, V, bool> r)
      {
        if (!Node.Receive (r))
        {
          return false;
        }

        return true;
      }

      internal sealed override PersistentHashMap<K, V> Set (uint h, int s, KeyValueNode<K, V> n)
      {
        var bit = Bit (h, s);
        if ((bit & Bitmap) != 0)
        {
          return new BitmapNode1<K, V> (Bitmap, Node.Set (h, s + TrieShift, n));
        }
        else if (Bitmap < bit)
        {
          return new BitmapNodeN<K,V> (Bitmap | bit, new PersistentHashMap<K, V> [] { Node, n });
        }
        else
        {
          return new BitmapNodeN<K,V> (bit | Bitmap, new PersistentHashMap<K, V> [] { n, Node });
        }
      }

      internal sealed override bool TryFind (uint h, int s, K k, out V v)
      {
        var bit = Bit (h, s);
        if ((bit & Bitmap) != 0)
        {
          return Node.TryFind (h, s + TrieShift, k, out v);
        }
        else
        {
          v = default (V);
          return false;
        }
      }

      internal sealed override PersistentHashMap<K, V> Unset (uint h, int s, K k)
      {
        var bit = Bit (h, s);
        if ((bit & Bitmap) != 0)
        {
          var localIdx  = PopCount (Bitmap & (bit - 1));
          var updated   = Node.Unset (h, s + TrieShift, k);
          if (ReferenceEquals (updated, Node))
          {
            return this;
          }
          else if (updated != null)
          {
            return new BitmapNode1<K, V> (Bitmap, updated);
          }
          else
          {
            return null;
          }
        }
        else
        {
          return this;
        }
      }
    }

    internal sealed partial class BitmapNodeN<K, V> : PersistentHashMap<K, V>
      where K : IEquatable<K>
    {
      public readonly uint              Bitmap  ;
      public readonly PersistentHashMap<K, V>[]  Nodes   ;

      [MethodImpl (MethodImplOptions.AggressiveInlining)]
      public BitmapNodeN (uint b, PersistentHashMap<K, V>[] ns)
      {
        Bitmap  = b ;
        Nodes   = ns;
      }

      [MethodImpl (MethodImplOptions.AggressiveInlining)]
      public static PersistentHashMap<K, V> FromTwoNodes (int s, uint h1, PersistentHashMap<K, V> n1, uint h2, PersistentHashMap<K, V> n2)
      {
        Debug.Assert (h1 != h2);
        Debug.Assert (s < TrieMaxShift);

        var b1 = Bit (h1, s);
        var b2 = Bit (h2, s);
        if (b1 < b2)
        {
          return new BitmapNodeN<K, V> (b1 | b2, new [] { n1, n2 });
        }
        else if (b1 > b2)
        {
          return new BitmapNodeN<K, V> (b2 | b1, new [] { n2, n1 });
        }
        else
        {
          return new BitmapNode1<K, V> (b1, FromTwoNodes (s + TrieShift, h1, n1, h2, n2));
        }
      }

      public override IEnumerator<KeyValuePair<K, V>> GetEnumerator()
      {
        foreach (var node in Nodes)
        {
          foreach (var kv in node)
          {
            yield return kv;
          }
        }
      }

#if PHM_TEST_BUILD
      internal override bool CheckInvariant (uint h, int s)
      {
        var length = PopCount (Bitmap);
        if (length < 2 || length != Nodes.Length)
        {
          return false;
        }

        var bitmap = Bitmap;

        var hash = -1;
        var iter = -1;

        while (bitmap != 0)
        {
          ++hash;

          var isSet = (bitmap & 0x1) != 0;
          bitmap >>= 1;

          if (!isSet)
          {
            continue;
          }

          ++iter;

          var n = Nodes[iter];
          if (n == null)
          {
            return false;
          }

          if (!n.CheckInvariant (h | (uint)(hash << s), s + TrieShift))
          {
            return false;
          }
        }

        return true;
      }

      internal override void Describe (StringBuilder sb, int indent)
      {
        var bits = Convert.ToString (Bitmap, 2);
        sb.FormatIndentedLine (indent, "BitmapN Bitmap:0b{0}, Nodes:{1}", new string ('0', 16 - bits.Length) + bits, Nodes.Length);
        for (var iter = 0; iter < Nodes.Length; ++iter)
        {
          var n = Nodes[iter];
          n.Describe (sb, indent + 2);
        }
      }
#endif

      internal override bool Receive (Func<K, V, bool> r)
      {
        for (var iter = 0; iter < Nodes.Length; ++iter)
        {
          if (!Nodes[iter].Receive (r))
          {
            return false;
          }
        }

        return true;
      }

      internal sealed override PersistentHashMap<K, V> Set (uint h, int s, KeyValueNode<K, V> n)
      {
        var bit = Bit (h, s);
        var localIdx = PopCount (Bitmap & (bit - 1));
        if ((bit & Bitmap) != 0)
        {
          var nvs = CopyArray (Nodes);
          nvs[localIdx] = Nodes[localIdx].Set (h, s + TrieShift, n);
          return new BitmapNodeN<K, V> (Bitmap, nvs);
        }
        else
        {
          var nvs = CopyArrayMakeHole (localIdx, Nodes, n);
          if (nvs.Length < TrieMaxNodes)
          {
            return new BitmapNodeN<K, V> (Bitmap | bit, nvs);
          }
          else
          {
            return new BitmapNode16<K, V> (nvs);
          }
        }
      }

      internal sealed override bool TryFind (uint h, int s, K k, out V v)
      {
        var bit = Bit (h, s);
        if ((bit & Bitmap) != 0)
        {
          var localIdx = PopCount (Bitmap & (bit - 1));
          return Nodes[localIdx].TryFind (h, s + TrieShift, k, out v);
        }
        else
        {
          v = default (V);
          return false;
        }
      }

      internal sealed override PersistentHashMap<K, V> Unset (uint h, int s, K k)
      {
        var bit = Bit (h, s);
        if ((bit & Bitmap) != 0)
        {
          var localIdx  = PopCount (Bitmap & (bit - 1));
          var updated   = Nodes[localIdx].Unset (h, s + TrieShift, k);
          if (ReferenceEquals (updated, Nodes[localIdx]))
          {
            return this;
          }
          else if (updated != null)
          {
            var nvs = CopyArray (Nodes);
            nvs[localIdx] = updated;
            return new BitmapNodeN<K, V> (Bitmap, nvs);
          }
          else if (Nodes.Length > 2)
          {
            return new BitmapNodeN<K, V> (Bitmap & ~bit, CopyArrayRemoveHole (localIdx, Nodes));
          }
          else if (Nodes.Length == 2)
          {
            return new BitmapNode1<K, V> (Bitmap & ~bit, Nodes[localIdx ^ 0x1]);
          }
          else
          {
            return null;
          }
        }
        else
        {
          return this;
        }
      }
    }

    internal sealed partial class BitmapNode16<K, V> : PersistentHashMap<K, V>
      where K : IEquatable<K>
    {
      public readonly PersistentHashMap<K, V>[]  Nodes   ;

      [MethodImpl (MethodImplOptions.AggressiveInlining)]
      public BitmapNode16 (PersistentHashMap<K, V>[] ns)
      {
        Nodes   = ns ;
      }

      public override IEnumerator<KeyValuePair<K, V>> GetEnumerator()
      {
        foreach (var node in Nodes)
        {
          foreach (var kv in node)
          {
            yield return kv;
          }
        }
      }

#if PHM_TEST_BUILD
      internal override bool CheckInvariant (uint h, int s)
      {
        if (TrieMaxNodes != Nodes.Length)
        {
          return false;
        }

        for (var iter = 0; iter < TrieMaxNodes; ++iter)
        {
          var n = Nodes[iter];
          if (n == null)
          {
            return false;
          }

          if (!n.CheckInvariant (h | (uint)(iter << s), s + TrieShift))
          {
            return false;
          }
        }

        return true;
      }

      internal override void Describe (StringBuilder sb, int indent)
      {
        sb.IndentedLine (indent, "Bitmap16");
        foreach (var node in Nodes)
        {
          node.Describe (sb, indent + 2);
        }
      }
#endif

      internal override bool Receive (Func<K, V, bool> r)
      {
        foreach (var node in Nodes)
        {
          if (!node.Receive (r))
          {
            return false;
          }
        }

        return true;
      }

      internal sealed override PersistentHashMap<K, V> Set (uint h, int s, KeyValueNode<K, V> n)
      {
        var localIdx  = (int)LocalHash (h, s);
        var nvs       = CopyArray (Nodes);
        nvs[localIdx] = Nodes[localIdx].Set (h, s + TrieShift, n);
        return new BitmapNode16<K, V> (nvs);
      }

      internal sealed override bool TryFind (uint h, int s, K k, out V v)
      {
        var localIdx  = (int)LocalHash (h, s);
        return Nodes[localIdx].TryFind (h, s + TrieShift, k, out v);
      }

      internal sealed override PersistentHashMap<K, V> Unset (uint h, int s, K k)
      {
        var localIdx  = (int)LocalHash (h, s);
        var updated   = Nodes[localIdx].Unset (h, s + TrieShift, k);
        if (ReferenceEquals (updated, Nodes[localIdx]))
        {
          return this;
        }
        else if (updated != null)
        {
          var nv        = Nodes[localIdx] ;
          var nvs       = CopyArray (Nodes);
          nvs[localIdx] = updated;
          return new BitmapNode16<K, V> (nvs);
        }
        else
        {
          var bit = Bit (h, s);
          return new BitmapNodeN<K, V> (TrieMask & ~bit, CopyArrayRemoveHole (localIdx, Nodes));
        }
      }
    }

    internal sealed partial class HashCollisionNodeN<K, V> : PersistentHashMap<K, V>
      where K : IEquatable<K>
    {
      public readonly uint                 Hash      ;
      public readonly KeyValueNode<K, V>[] KeyValues ;

      [MethodImpl (MethodImplOptions.AggressiveInlining)]
      public HashCollisionNodeN (uint h, KeyValueNode<K, V>[] kvs)
      {
        Hash      = h   ;
        KeyValues = kvs ;
      }

      [MethodImpl (MethodImplOptions.AggressiveInlining)]
      public static HashCollisionNodeN<K, V> FromTwoNodes (uint h, KeyValueNode<K, V> kv1, KeyValueNode<K, V> kv2)
      {
        return new HashCollisionNodeN<K, V> (h, new [] { kv1, kv2 });
      }

      public override IEnumerator<KeyValuePair<K, V>> GetEnumerator()
      {
        foreach (var kv in KeyValues)
        {
          yield return new KeyValuePair<K, V> (kv.Key, kv.Value);
        }
      }

#if PHM_TEST_BUILD
      internal override bool CheckInvariant (uint h, int s)
      {
        if (KeyValues.Length < 2)
        {
          return false;
        }

        for (var iter = 0; iter < KeyValues.Length; ++iter)
        {
          var kv = KeyValues[iter];
          var k = kv.Key;
          if ((uint)k.GetHashCode () != Hash)
          {
            return false;
          }

          if (!kv.CheckInvariant (h, s))
          {
            return false;
          }
        }

        return CheckHash (Hash, h, s);
      }

      internal override void Describe (StringBuilder sb, int indent)
      {
        sb.FormatIndentedLine (indent, "HashCollison Hash:0x{0:X08}, KeyValues:{1}", Hash, KeyValues.Length);
        for (var iter = 0; iter < KeyValues.Length; ++iter)
        {
          var kv = KeyValues[iter];
          kv.Describe (sb, indent + 2);
        }
      }
#endif

      internal override bool Receive (Func<K, V, bool> r)
      {
        for (var iter = 0; iter < KeyValues.Length; ++iter)
        {
          var kv = KeyValues[iter];
          if (!r (kv.Key, kv.Value))
          {
            return false;
          }
        }

        return true;
      }

      internal sealed override PersistentHashMap<K, V> Set (uint h, int s, KeyValueNode<K, V> n)
      {
        if (Hash == h)
        {
          var k = n.Key;
          for (var iter = 0; iter < KeyValues.Length; ++iter)
          {
            if (KeyValues[iter].Key.Equals (k))
            {
              var rvs = CopyArray (KeyValues);
              rvs[iter] = n;
              return new HashCollisionNodeN<K, V> (h, rvs);
            }
          }

          return new HashCollisionNodeN<K, V> (h, CopyArrayMakeHoleLast (KeyValues, n));
        }
        else
        {
          return BitmapNodeN<K, V>.FromTwoNodes (s, Hash, this, h, n);
        }
      }

      internal sealed override bool TryFind (uint h, int s, K k, out V v)
      {
        if (Hash == h)
        {
          for (var iter = 0; iter < KeyValues.Length; ++iter)
          {
            var kv = KeyValues[iter];
            if (kv.Key.Equals (k))
            {
              v = kv.Value;
              return true;
            }
          }

          v = default (V);
          return false;
        }
        else
        {
          v = default (V);
          return false;
        }
      }

      internal override PersistentHashMap<K, V> Unset (uint h, int s, K k)
      {
        if (Hash == h)
        {
          for (var iter = 0; iter < KeyValues.Length; ++iter)
          {
            if (KeyValues[iter].Key.Equals (k))
            {
              if (KeyValues.Length > 2)
              {
                return new HashCollisionNodeN<K, V> (h, CopyArrayRemoveHole (iter, KeyValues));
              }
              if (KeyValues.Length == 2)
              {
                return KeyValues[iter ^ 0x1];
              }
              else
              {
                return null;
              }
            }
          }

          return this;
        }
        else
        {
          return this;
        }
      }
    }
  }
}
