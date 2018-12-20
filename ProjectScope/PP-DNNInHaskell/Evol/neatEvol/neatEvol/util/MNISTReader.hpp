//
//  MNISTReader.hpp
//  neatEvol
//
//  Created by RHVT on 20/12/18.
//  Copyright © 2018 RHVT. All rights reserved.
//

#pragma once

#include "std.h"

uint **read_mnist_images(std::string full_path, int &number_of_images, int &image_size);
uint *read_mnist_labels(std::string full_path, int &number_of_labels);
