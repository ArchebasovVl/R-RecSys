#!/bin/bash

DATA_PATH=./data

curl -L -o $DATA_PATH/fashion-products.zip https://www.kaggle.com/api/v1/datasets/download/bhanupratapbiswas/fashion-products
unzip $DATA_PATH/fashion-products.zip -d $DATA_PATH && rm $DATA_PATH/fashion-products.zip

