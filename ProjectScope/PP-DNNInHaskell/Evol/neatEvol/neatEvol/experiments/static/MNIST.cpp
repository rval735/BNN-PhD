#include "std.h" // Must be included first. Precompiled header with standard library includes.
#include "staticexperiment.h"
#include "MNISTReader.hpp"

using namespace NEAT;
using namespace std;

struct ImgM {
    int lbl;
    vector<uint> img;
};

vector<ImgM> loadMNIST() {
    cout << "About to load files";
    std::string pathLbls = "t10k-labels-idx1-ubyte";
    std::string pathImgs = "t10k-images-idx3-ubyte";
    int numLabels = 100;
    int numImgs = 100;
    int imgSize = 100;
    uint *lbls = read_mnist_labels(pathLbls, numLabels);
    uint **imgs = read_mnist_images(pathImgs, numImgs, imgSize);
    
    if (numLabels != numImgs) {
        cout << "Labels and images do not match";
        exit(EXIT_FAILURE);
    }
    
    vector<ImgM> dataset = vector<ImgM>(numLabels);
    
    for (int i = 0; i < numLabels; i++) {
        vector<uint> vals = vector<uint>(imgSize);
        
        for (int j = 0; j < imgSize; j++) {
            vals[j] = imgs[i][j];
//            cout << i << "," << j << ":" << vals[j] << "\n";
        }
        
        ImgM elem;
        elem.lbl = lbls[i];
        elem.img = vals;
        dataset.push_back(elem);
    }
    
    delete lbls;
    delete[] imgs;
    
    return dataset;
}

static struct MNISTInit {
    MNISTInit() {
        create_static_experiment("MNIST", [] () {
            
            static uint batch = 0;
            static vector<ImgM> fullDataset = vector<ImgM>();
            
            const real_t weight = 1.0;
            const real_t T = 1;
            const real_t F = 0;
            const uint batchSize = 50;
            
            if (fullDataset.empty()) {
                fullDataset = loadMNIST();
            }

            vector<ImgM>::const_iterator subBegin = fullDataset.begin() + (batchSize * batch);
            vector<ImgM>::const_iterator subEnd = fullDataset.begin() + (batchSize + batchSize * batch);
            vector<ImgM> subDataset(subBegin, subEnd);
//            vector<Test> tests = vector<Test>(batchSize);
            
//            for (int i = 0; i < batchSize; i++) {
//                Step stp = Step(
//
//                tests[i] = {{
//                    {{1, 1}, {1}, weight},
//                }};
//            }
            
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
            
            cout << "--------";
            cout << "Batch: " << batch;
            batch++;
            
            return tests;
        });
    }
} init;


