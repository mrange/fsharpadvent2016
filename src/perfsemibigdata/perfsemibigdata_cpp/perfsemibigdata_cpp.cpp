// perfsemibigdata_cpp.cpp : Defines the entry point for the console application.
//

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

    auto w = (long long) (v);
    auto z = (w / alignment + (w % alignment ? 1 : 0)) * alignment;
    return (t) (z);
  }

  struct particles
  {
    particles (std::size_t particle_count)
      : a_is_current    (true)
      , particle_count  (particle_count)
    {
      auto pc = particle_count + 4 + 4; // + 4 (if particle_count % 4 <> 0) + 4 (if alignment % 32 <> 0) 

      a.x.resize (pc);
      a.y.resize (pc);
      a.z.resize (pc);
      b.x.resize (pc);
      b.y.resize (pc);
      b.z.resize (pc);
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

      verlet_axis (acceleration_x, current.x, previous.x);
      verlet_axis (acceleration_y, current.y, previous.y);
      verlet_axis (acceleration_z, current.z, previous.z);
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
    void verlet_axis (
        double acceleration
      , std::vector<double> & a
      , std::vector<double> & b
    )
    {
      auto c_p = realign<32> (&a.front ());
      auto p_p = realign<32> (&b.front ());

      auto sz4 = realign<4> (particle_count) / 4;

      for (auto iter = sz4; iter > 0; --iter)
      {
        // p = c + c - p + acc
        //_mm_prefetch((const char*)(c_p + 8), _MM_HINT_NTA);
        //_mm_prefetch((const char*)(p_p + 8), _MM_HINT_NTA);
        auto const c    = _mm256_load_pd (c_p);
        auto const p    = _mm256_load_pd (p_p);
        auto old_p      = p_p;
        c_p += 4;
        p_p += 4;
        auto const acc  = _mm256_set1_pd (acceleration);
        _mm256_store_pd (old_p, _mm256_add_pd (_mm256_sub_pd (_mm256_add_pd (c, c), p), acc));
      }
    }

    bool        a_is_current  ;
    std::size_t particle_count;

    struct positions
    {
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

}

int main()
{
/*
  std::printf ("Starting...\n");

  ps.verlet (0, 1, 0);
  ps.verlet (0, 0, 0);

  auto vs   = ps.current_vertices ();
  auto sum  = std::accumulate (vs.begin (), vs.end (), vector3::zero);

  std::printf ("Sum: %f, %f, %f\n", sum.x, sum.y, sum.z);
  */

  std::vector<double> results;

  for (auto inner : inners)
  {
    std::size_t count = 10000000;
    std::size_t outer = count / inner;

    std::printf ("Running test cases with outer=%zd, inner=%zd\n", outer, inner);

    particles ps (inner);

    auto ms = time_it (outer, [&ps] { ps.verlet (0, 1, 0); });
    results.push_back (ms);

    std::printf (" It tooks %f ms\n", ms);
  }

  printf ("Structures of Arrays (AVX)");

  for (auto ms : results)
  {
    printf (",%f", ms);
  }

  return 0;
}

