#include "std.h" // Must be included first. Precompiled header with standard library includes.
#include "staticexperiment.h"

using namespace NEAT;
using namespace std;

static struct XNorInit {
    XNorInit() {
        create_static_experiment("xnor", [] () {
                const real_t T = 1.0;
                const real_t F = 0.0;
                const real_t weight = 1.0;

                vector<Test> tests = {
                    {{
                            {{F, F}, {T}, weight},
                    }},
                    {{
                            {{F, T}, {F}, weight},
                    }},
                    {{
                            {{T, F}, {F}, weight},
                    }},
                    {{
                            {{T, T}, {T}, weight}
                    }}
                };

                return tests;
            });
    }
} init;
