#include "stdafx.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>


#include <algorithm>
#include <chrono>
#include <numeric>
#include <vector>
#include <type_traits>

#include "immintrin.h"
#include "emmintrin.h"

namespace
{
  struct vector3
  {
    double x;
    double y;
    double z;

    static vector3 const zero;
  };

  vector3 const vector3::zero = {0, 0, 0};

  vector3 operator+ (vector3 const & l, vector3 const & r)
  {
    return {l.x + r.x, l.y + r.y, l.z + r.z };
  }

  template<std::size_t alignment, typename T>
  T realign (T && v)
  {
    static_assert (alignment > 0                    , "alignment > 0");
    static_assert (sizeof (long long) >= sizeof (T) , "sizeof (long long) >= sizeof (T)");

    using t = std::decay_t<T>;
    static_assert (std::is_pointer<t>::value || std::is_integral<t>::value , "std::is_pointer<t>::value || std::is_integral<t>::value");

    auto w = (long long) v;
    auto z = ((w + alignment - 1) / alignment) * alignment;
    return (t) z;
  }

  struct avx
  {
    constexpr static std::size_t const stride     = 4;
    constexpr static std::size_t const alignment  = stride * sizeof (double);
    constexpr static char const        name []    = "AVX";

    static inline __m256d load (double const * p)
    {
      return _mm256_load_pd (p);
    }

    static inline void store (double * p, __m256d v)
    {
      return _mm256_store_pd (p, v);
    }

    static inline __m256d set (double v)
    {
      return _mm256_set1_pd (v);
    }

    static inline __m256d add (__m256d a, __m256d b)
    {
      return _mm256_add_pd (a, b);
    }

    static inline __m256d sub (__m256d a, __m256d b)
    {
      return _mm256_sub_pd (a, b);
    }
  };

  struct sse2
  {
    constexpr static std::size_t const  stride    = 2;
    constexpr static std::size_t const  alignment = stride * sizeof (double);
    constexpr static char const         name []   = "SSE2";

    static inline __m128d load (double const * p)
    {
      return _mm_load_pd (p);
    }

    static inline void store (double * p, __m128d v)
    {
      return _mm_store_pd (p, v);
    }

    static inline __m128d set (double v)
    {
      return _mm_set1_pd (v);
    }

    static inline __m128d add (__m128d a, __m128d b)
    {
      return _mm_add_pd (a, b);
    }

    static inline __m128d sub (__m128d a, __m128d b)
    {
      return _mm_sub_pd (a, b);
    }
  };

  template<typename TAdapter>
  struct particles
  {
    constexpr static std::size_t const stride     = TAdapter::stride ;
    constexpr static std::size_t const alignment  = TAdapter::alignment   ;

    particles (std::size_t particle_count)
      : a_is_current    (true)
      , particle_count  (particle_count)
      , a (particle_count + stride + stride) // + double_count (if particle_count % double_count <> 0) + double_count (if ptr % alignment <> 0)
      , b (particle_count + stride + stride) // + double_count (if particle_count % double_count <> 0) + double_count (if ptr % alignment <> 0)
    {
    }

    void verlet (
        double acceleration_x
      , double acceleration_y
      , double acceleration_z
      )
    {
      auto & current  = a_is_current ? a : b;
      auto & previous = a_is_current ? b : a;

      a_is_current = !a_is_current;

      verlet_axis (particle_count, acceleration_x, current.x, previous.x);
      verlet_axis (particle_count, acceleration_y, current.y, previous.y);
      verlet_axis (particle_count, acceleration_z, current.z, previous.z);
    }

    std::vector<vector3> current_vertices () const
    {
      auto & current  = a_is_current ? a : b;

      assert (current.x.size () == current.y.size ());
      assert (current.x.size () == current.z.size ());

      auto size = current.x.size ();

      std::vector<vector3> result;
      result.reserve (size);

      for (auto iter = 0; iter < size; ++iter)
      {
        result.push_back ({current.x[iter], current.y[iter], current.z[iter]});
      }

      return result;
    }

  private:
    static void verlet_axis (
        std::size_t           particle_count
      , double                acceleration
      , std::vector<double> & a
      , std::vector<double> & b
    )
    {
      auto c_p = realign<alignment> (&a.front ());
      auto p_p = realign<alignment> (&b.front ());

      auto sz = realign<stride> (particle_count) / stride;

      for (auto iter = sz; iter > 0; --iter)
      {
        // p = c + c - p + acc
        //_mm_prefetch((const char*)(c_p + 8), _MM_HINT_NTA);
        //_mm_prefetch((const char*)(p_p + 8), _MM_HINT_NTA);
        auto const c    = TAdapter::load (c_p);
        auto const p    = TAdapter::load (p_p);
        auto old_p      = p_p;
        c_p += stride;
        p_p += stride;
        auto const acc  = TAdapter::set (acceleration);
        TAdapter::store (old_p, TAdapter::add (TAdapter::sub (TAdapter::add (c, c), p), acc));
      }
    }

    bool        a_is_current  ;
    std::size_t particle_count;

    struct positions
    {
      positions (std::size_t particle_count)
      {
        assert (particle_count > 0);
        assert (particle_count % double_count == 0);
        x.resize (particle_count);
        y.resize (particle_count);
        z.resize (particle_count);
      }

      std::vector<double> x ;
      std::vector<double> y ;
      std::vector<double> z ;
    };

    positions a;
    positions b;
  };

  std::size_t const inners [] = {100,110,120,132,145,159,175,192,210,231,254,278,305,335,368,404,443,486,534,586,643,705,774,850,933,1024,1123,1233,1353,1485,1630,1789,1963,2154,2364,2595,2848,3126,3430,3765,4132,4535,4977,5462,5995,6579,7221,7925,8697,9545,10476,11498,12619,13849,15199,16681,18307,20092,22051,24201,26561,29151,31993,35112,38535,42292,46416,50941,55908,61359,67342,73907,81113,89022,97701,107227,117681,129155,141747,155568,170735,187382,205651,225702,247708,271859,298365,327455,359381,394421,432876,475081,521401,572237,628029,689261,756463,830218,1000000};

  template<typename TAction>
  double time_it (std::size_t n, TAction && action)
  {
    action ();

    auto before = std::chrono::high_resolution_clock::now ();

    for (auto iter = 0; iter < n; ++iter)
    {
      action ();
    }

    auto now    = std::chrono::high_resolution_clock::now ();;
    auto diff   = now - before;

    auto us     = std::chrono::duration_cast<std::chrono::microseconds> (diff);

    return us.count () / 1000.0;
  }

  template<typename TAdapter>
  void run ()
  {
    std::vector<double> results;

    for (auto inner : inners)
    {
      std::size_t count = 10000000;
      std::size_t outer = count / inner;

      std::printf ("Running test cases with outer=%zd, inner=%zd\n", outer, inner);

      particles<TAdapter> ps (inner);

      auto ms = time_it (outer, [&ps] { ps.verlet (0, 1, 0); });
      results.push_back (ms);

      std::printf (" It tooks %f ms\n", ms);
    }

    printf ("Structures of Arrays (%s)", TAdapter::name);

    for (auto ms : results)
    {
      printf (",%f", ms);
    }

    printf ("\n");
  }

}

int main()
{
  run<avx> ();

  return 0;
}

