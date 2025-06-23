# DDoS Attack Detection using Machine Learning in R

## 📋 Overview

This project implements machine learning algorithms to detect Distributed Denial of Service (DDoS) attacks in network traffic using R. The system analyzes network flow features to classify traffic as either benign or malicious (DDoS attack).

## 📁 Project Structure

```
ddos-detection-r/
│
├── Predict_DDoS.R          # Main analysis script
├── README.md               # Project documentation
├── data/                   # Dataset directory
│   └── Friday-WorkingHours-Afternoon-DDos.pcap_ISCX.csv
├── plots/                  # Generated visualizations
└── results/                # Model outputs and metrics
```

## 🎯 Objectives

- Analyze network traffic patterns to identify DDoS attacks
- Compare the performance of different machine learning algorithms
- Provide visualization of network traffic characteristics
- Build reliable classification models for cybersecurity applications

## 📊 Dataset

The project uses the **CICIDS2017 dataset**, specifically the Friday afternoon DDoS traffic capture:
- File: `Friday-WorkingHours-Afternoon-DDos.pcap_ISCX.csv`
- Contains network flow features extracted from packet captures
- Binary classification: Normal traffic (0) vs DDoS traffic (1)

### Key Features Analyzed:
- `Flow.Duration` - Duration of network flow
- `Total.Fwd.Packets` - Total forward packets
- `Total.Backward.Packets` - Total backward packets  
- `Total.Length.of.Fwd.Packets` - Total length of forward packets
- `Total.Length.of.Bwd.Packets` - Total length of backward packets
- `Fwd.Packet.Length.Max` - Maximum forward packet length
- `Bwd.Packet.Length.Max` - Maximum backward packet length
- `Fwd.IAT.Mean` - Mean inter-arrival time for forward packets
- `Bwd.IAT.Mean` - Mean inter-arrival time for backward packets

## 🛠️ Technologies Used

### R Packages:
- `readr` - Data import
- `tidyverse` - Data manipulation and analysis
- `ggplot2` - Data visualization
- `corrplot` - Correlation matrix visualization
- `caret` - Machine learning framework
- `ROCR` - Model evaluation
- `randomForest` - Random Forest algorithm
- `dplyr` - Data manipulation

## 📈 Analysis Pipeline

### 1. Data Exploration
- **Descriptive Statistics**: Summary statistics for all variables
- **Missing Values Analysis**: Identification and handling of missing data
- **Distribution Analysis**: Traffic distribution between normal and DDoS classes

### 2. Data Visualization
- **Pie Chart**: Overall traffic distribution (Normal vs DDoS)
- **Histograms**: Distribution of key network features
- **Density Plots**: Feature distributions by traffic type
- **Correlation Matrix**: Relationships between variables

### 3. Machine Learning Models

#### Logistic Regression
- Binary classification using selected features
- Cross-validation for model validation
- Feature selection based on correlation analysis

#### Random Forest
- Ensemble learning approach
- 500 trees with mtry=3
- Feature importance analysis
- Robust to overfitting

### 4. Model Evaluation
- **Confusion Matrix**: Classification accuracy metrics
- **Precision, Recall, F1-Score**: Detailed performance metrics
- **Cross-validation**: 5-fold cross-validation for reliability


## 📊 Results

The project implements multiple approaches:

1. **Simple Logistic Regression**: Uses 3 key features for quick classification
2. **Enhanced Logistic Regression**: Includes feature selection and correlation analysis
3. **Random Forest**: Ensemble method for improved accuracy

### Model Performance Metrics:
- Accuracy
- Precision
- Recall
- F1-Score
- Specificity
- Sensitivity

## 🔍 Key Features

- **Automated Data Preprocessing**: Handles missing values and feature selection
- **Comprehensive Visualization**: Multiple chart types for data exploration
- **Multiple ML Algorithms**: Comparison between different approaches
- **Robust Evaluation**: Cross-validation and detailed metrics
- **Reproducible Results**: Set seed for consistent outcomes



## 📧 Contact

Email - jlanin03@gmail.com

---
