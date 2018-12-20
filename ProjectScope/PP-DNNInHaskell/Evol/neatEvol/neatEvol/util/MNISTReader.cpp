//
//  MNISTReader.cpp
//  neatEvol
//
//  Created by RHVT on 20/12/18.
//  Copyright © 2018 RHVT. All rights reserved.
//
// Source:
// https://stackoverflow.com/questions/8286668/how-to-read-mnist-data-in-c
// http://yann.lecun.com/exdb/mnist/

#include "MNISTReader.hpp"

using namespace std;

int reverseInt (int i)
{
    unsigned char c1, c2, c3, c4;
    
    c1 = i & 255;
    c2 = (i >> 8) & 255;
    c3 = (i >> 16) & 255;
    c4 = (i >> 24) & 255;
    
    return ((int)c1 << 24) + ((int)c2 << 16) + ((int)c3 << 8) + c4;
}

uint32_t swap_endian(uint32_t val) {
    val = ((val << 8) & 0xFF00FF00) | ((val >> 8) & 0xFF00FF);
    return (val << 16) | (val >> 16);
}

uint **read_mnist_images(string full_path, int &number_of_images, int &image_size) {
   
    ifstream file(full_path, ios::binary);
    
    if(file.is_open()) {
        int magic_number = 0, n_rows = 0, n_cols = 0;
        
        file.read((char *)&magic_number, sizeof(magic_number));
        magic_number = reverseInt(magic_number);
        
        if(magic_number != 2051) throw runtime_error("Invalid MNIST image file!");
        
        file.read((char *)&number_of_images, sizeof(number_of_images));
        number_of_images = reverseInt(number_of_images);
        file.read((char *)&n_rows, sizeof(n_rows));
        n_rows = reverseInt(n_rows);
        file.read((char *)&n_cols, sizeof(n_cols));
        n_cols = reverseInt(n_cols);
        
        image_size = n_rows * n_cols;
        
        uint **dataset = new uint *[number_of_images];
        unsigned char img[image_size];
// 0,203:185
        for(int i = 0; i < number_of_images; i++) {
            dataset[i] = new uint[image_size];
            file.read((char *)img, image_size);
            
            for(int j = 0; j < image_size; j++){
                dataset[i][j] = static_cast<uint>(img[j]);
//                cout << i << "," << j << ":" << dataset[i][j] << "\n";
            }
        }
        
        file.close();
        
        return dataset;
    } else {
        throw runtime_error("Cannot open file `" + full_path + "`!");
    }
}

uint *read_mnist_labels(string full_path, int &number_of_labels) {
   
    ifstream file(full_path, ios::binary);
    
    if(file.is_open()) {
        int magic_number = 0;
        file.read((char *)&magic_number, sizeof(magic_number));
        magic_number = reverseInt(magic_number);
        
        if(magic_number != 2049) throw runtime_error("Invalid MNIST label file!");
        
        file.read((char *)&number_of_labels, sizeof(number_of_labels));
        number_of_labels = reverseInt(number_of_labels);
        
        uint *dataset = new uint[number_of_labels];
        char dChar = 'c';
        for(int i = 0; i < number_of_labels; i++) {
            file.read(&dChar, 1);
            dataset[i] = uint(dChar);
        }
        
        file.close();
        
        return dataset;
    } else {
        throw runtime_error("Unable to open file `" + full_path + "`!");
    }
}
