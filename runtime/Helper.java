package fula;

class helper {

    interface IO  {
        void unsafeRunSync();
    }

    static IO printFula(final int num) {
        return new IO() {
            public void unsafeRunSync() {
                System.out.print(num);
            }
        };
    };

    static IO printlnFula(final int num) {
        return new IO() {
            public void unsafeRunSync() {
                System.out.println(num);
            }
        };
    };

    static IO printFula(final float num) {
        return new IO() {
            public void unsafeRunSync() {
                System.out.print(num);
                return;
            }
        };
    };
    static IO printlnFula(final float num) {
        return new IO() {
            public void unsafeRunSync() {
                System.out.println(num);
                return;
            }
        };
    };

    static IO printFula(final boolean bool) {
        return new IO() {
            public void unsafeRunSync() {
                System.out.print(bool);
                return;
            }
        };
    };

    static IO printlnFula(final boolean bool) {
        return new IO() {
            public void unsafeRunSync() {
                System.out.println(bool);
                return;
            }
        };
    };

    static IO printFula(final String bool) {
        return new IO() {
            public void unsafeRunSync() {
                System.out.print(bool);
                return;
            }
        };
    };

    static IO printlnFula(final String bool) {
        return new IO() {
            public void unsafeRunSync() {
                System.out.println(bool);
                return;
            }
        };
    };

    static IO join(IO a, IO b) {
        return new IO() {
            public void unsafeRunSync() {
                a.unsafeRunSync();
                b.unsafeRunSync();
                return;
            }
        };
    }
}