/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure;

import clojure.lang.ExceptionInfo;
import clojure.lang.IMapEntry;
import clojure.lang.IPersistentMap;
import clojure.lang.PersistentHashMap;

public class main{

    public static class Random {
        static final long m = 0x7FFFFFFF;
        static final long a = 48271;
        static final long c = 0;
        static final double d = 1.0 / m;
        long state;

        public Random (int seed) {
            state = seed;
        }

        public int next(int b, int e) {
            state = (a*state + c) % m;
            double r = state*d;
            double v = (e - b)*r + b;
            return (int)v;
        }

    }

    public static class Key {
        final int v;

        public Key(int v) {
            this.v = v;
        }

        public int get() {
            return v;
        }

        @Override
        public int hashCode() {
            return v;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof Key) {
                return ((Key)obj).v == this.v;
            } else {
                return false;
            }
        }

        @Override
        public String toString() {
            return String.format("%d", v);
        }
    }

    public static class TestCase {
        public final String   name    ;
        public final Runnable action  ;

        public TestCase(String name, Runnable action)
        {
            this.name   = name;
            this.action = action;
        }

    }
    public static class PerformanceTest {

        Random random                   = new Random(19740531);
        final int total                 = 4000000;
        final int outer                 = 4000;
        final int inner                 = total / outer;
        final int mult                  = 4;

        final Object[]  inserts         = new Object[inner*2];
        final Object[]  removals        = new Object[inner*2];;

        final IPersistentMap builtUp ;
        final IPersistentMap empty   ;
        final IPersistentMap inserted;

        IPersistentMap doBuildUp () {
            return PersistentHashMap.create(inserts);
        }

        IPersistentMap doInsert (IPersistentMap pm) {
            for (int iter = 0; iter < inserts.length; iter += 2)
            {
                pm = pm.assoc(inserts[iter + 0], inserts[iter + 1]);
            }
            return pm;
        }

        <T> long time (int repeat, Runnable runnable) {
            runnable.run();
            long longTimeAgo = System.nanoTime();
            for (int iter = 0; iter < repeat; ++iter) {
                runnable.run();
            }
            long rightNow    = System.nanoTime();
            return (rightNow - longTimeAgo) / 1000000;
        }

        static TestCase tc (String name, Runnable runnable) {
            return new TestCase(name, runnable);
        }

        void lookup(IPersistentMap pm) {
            boolean allFound = true;
            for (int iter = 0; iter < inserts.length; iter += 2) {
                Object k        = inserts[iter + 0];
                Object v        = inserts[iter + 1];
                IMapEntry me    = pm.entryAt(k);
                allFound        &= me != null;
            }
            if (!allFound) {
                throw new RuntimeException("Expected all to be found");
            }
        }

        void buildUp() {
            IPersistentMap pm = doBuildUp();
            if (pm.count() != inserted.count()) {
                throw new RuntimeException("Expected count to be equal to inserted");
            }
        }

        void insert(IPersistentMap pm) {
            for (int iter = 0; iter < inserts.length; iter += 2) {
                Object k        = inserts[iter + 0];
                Object v        = inserts[iter + 1];
                pm              = pm.assoc(k, v);
            }
            if (pm.count() != inserted.count()) {
                throw new RuntimeException("Expected count to be equal to inserted");
            }
        }

        int crcFor(Object[] kvs) {
            int crc = 0;
            for (int iter = 0; iter < kvs.length; iter += 2) {
                int k = Integer.parseInt(kvs[iter + 0].toString());
                int v = Integer.parseInt(kvs[iter + 1].toString());
                crc += (iter/2)^k^v;
            }
            return crc;
        }

        public PerformanceTest () {
            for (int iter = 0; iter < inner; ++iter) {
                inserts[2*iter + 0] = new Key(random.next(0, inner*mult));
                inserts[2*iter + 1] = Integer.valueOf(iter).toString();
            }
            for (int iter = 0; iter < inner; ++iter) {
                removals[2*iter + 0] = inserts[2*iter + 0];
                removals[2*iter + 1] = inserts[2*iter + 1];
            }
            for (int iter = 0; iter < inner - 1; ++iter) {
                int swap             = random.next(iter, inner);
                Object k             = removals[2*swap + 0];
                Object v             = removals[2*swap + 1];
                removals[2*swap + 0] = removals[2*iter + 0];
                removals[2*swap + 1] = removals[2*iter + 1];
                removals[2*iter + 0] = k;
                removals[2*iter + 1] = v;
            }

            builtUp     = doBuildUp();
            empty       = PersistentHashMap.EMPTY;
            inserted    = doInsert(empty);

            System.out.format("CRC for inserts : %d\n", crcFor(inserts));
            System.out.format("CRC for removals: %d\n", crcFor(removals));

            System.out.println(inserted.toString());
            System.out.println(builtUp.toString());

            TestCase[] testCases = new TestCase[] {
//                tc ("Lookup BuiltUp"    , () -> this.lookup(builtUp)),
                tc ("Lookup Inserted"   , () -> this.lookup(inserted)),
//                tc ("BuildUp"           , () -> this.buildUp()),
                tc ("Insert"            , () -> this.insert(inserted)),
            };

            for (TestCase tc : testCases) {
                System.out.format("Running %s...\n", tc.name);
                long tm = time(outer, tc.action);
                System.out.format("... took %d ms\n", tm);
            }

        }
    }

    public static void main(String[] args)
    {
        PerformanceTest pt = new PerformanceTest();
    }
}
