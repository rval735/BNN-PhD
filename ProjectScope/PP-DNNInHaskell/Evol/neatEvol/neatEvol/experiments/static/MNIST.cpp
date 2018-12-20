#include "std.h" // Must be included first. Precompiled header with standard library includes.
#include "staticexperiment.h"
#include "MNISTReader.hpp"

using namespace NEAT;
using namespace std;

void loadMNIST() {
    cout << "About to load files";
    std::string pathLbls = "t10k-labels-idx1-ubyte";
    std::string pathImgs = "t10k-images-idx3-ubyte";
    int numLabels = 100;
    int numImgs = 100;
    int imgSize = 100;
    uint *lbls = read_mnist_labels(pathLbls, numLabels);
    uint **imgs = read_mnist_images(pathImgs, numImgs, imgSize);
    cout << "Labels:" << lbls;
    cout << "Imgs:" << imgs;
}

static struct MNISTInit {
    MNISTInit() {
        create_static_experiment("MNIST", [] () {
            const real_t T = 1.0;
            const real_t F = 0.0;
            const real_t weight = 1.0;
            
            loadMNIST();
            vector<Test> tests = {
                {{
                    {{F, F}, {F}, weight},
                }},
                {{
                    {{F, T}, {T}, weight},
                }},
                {{
                    {{T, F}, {T}, weight},
                }},
                {{
                    {{T, T}, {F}, weight}
                }}
            };
            
            return tests;
        });
    }
} init;



