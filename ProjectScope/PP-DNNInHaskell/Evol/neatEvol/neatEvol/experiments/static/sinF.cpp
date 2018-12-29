#include "std.h" // Must be included first. Precompiled header with standard library includes.
#include "staticexperiment.h"

using namespace NEAT;
using namespace std;

Test f(int i, real_t weight)
{
    std::ostringstream nameSt;
    nameSt << "Test-" << i;
    vector<Step> stp;
    real_t input = static_cast<real_t>(i);
    vector<real_t> inputs(1, input);
    real_t output = cos(input);
    vector<real_t> outputs(1, output);
    stp.push_back(Step(inputs, outputs, weight));
    return Test(nameSt.str(), stp);
}

static struct CosFInit {
    CosFInit() {
        create_static_experiment("cosF", [] () {
            
            vector<Test> tests;
            const real_t weight = 1.0;
            
            for (int i = 0; i < 15; i++) {
                tests.push_back(f(i, weight));
            }
            
            return tests;
        });
    }
} init;
