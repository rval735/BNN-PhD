#include "std.h" // Must be included first. Precompiled header with standard library includes.
#include "staticexperiment.h"
#include "MNISTReader.hpp"

using namespace NEAT;
using namespace std;

struct ImgM {
    real_t lbl;
    vector<real_t> img;
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
    uint dataSetReduction = 4;
    
    for (int i = 0; i < numLabels; i++) {
        
        vector<real_t> vals = vector<real_t>(imgSize / dataSetReduction);
        
        for (uint j = 0; j < imgSize / dataSetReduction; j++) {
            real_t val = (imgs[i][j] + imgs[i][j + 1] + imgs[i][j + 2] + imgs[i][j + 3]) / dataSetReduction;
            vals[j] = val;
//            cout << i << "," << j << ":" << vals[j] << "\n";
        }
        
        ImgM elem;
        elem.lbl = static_cast<real_t>(lbls[i]);
        elem.img = vals;
        dataset[i] = elem;
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
            const uint batchSize = 50;
            
            if (fullDataset.empty()) {
                fullDataset = loadMNIST();
            }

            vector<ImgM>::const_iterator subBegin = fullDataset.begin() + (batchSize * batch);
            vector<ImgM>::const_iterator subEnd = fullDataset.begin() + (batchSize + batchSize * batch);
            vector<ImgM> subDataset(subBegin, subEnd);
//            vector<Step> steps = vector<Step>(batchSize);
            vector<Test> tests; // = vector<Test>(batchSize);
            std::ostringstream nameSt;
            
            nameSt << "MNIST-Batch:" << batchSize * batch;
            
            for (uint i = 0; i < batchSize; i++) {
                ImgM elem = subDataset[i];
                vector<real_t> oneLbl = vector<real_t>(1);
                oneLbl[0] = elem.lbl;
                vector<Step> stp;
                stp.push_back(Step(elem.img, oneLbl, weight));
//                cout.setf(ios::fixed);
//                cout << stp[0].input[0] << "--" << stp[0].output[0] << "--" << stp[0].weight;
                tests.push_back(Test(nameSt.str(), stp));
            }
            
//            const real_t T = 1.0;
//            const real_t F = 1.0;
//            vector<Test> tests0 = {
//                {{
//                    {{F, F}, {F}, weight},
//                }},
//                {{
//                    {{F, T}, {T}, weight},
//                }},
//                {{
//                    {{T, F}, {T}, weight},
//                }},
//                {{
//                    {{T, T}, {F}, weight}
//                }}
//            };
            
            cout << "--------";
            cout << "Batch: " << batch;
            batch++;
            
            return tests;
        });
    }
} init;

