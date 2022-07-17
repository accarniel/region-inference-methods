# Overview 

This repository maintains the implementation of the comparative analysis conducted in the following research paper:

- Carniel, A. C; Galdino, F.; Schneider, M. Evaluating Region Inference Methods by Using Fuzzy Spatial Inference Models. In: 2022 IEEE International Conference on Fuzzy Systems (FUZZ-IEEE), 2022, pp. 1-8.

The goal of the comparative analysis is to understand the properties, similarities, and relationships of two **Region Inference Methods**:
- _discretization method_
- _optimization method_

These methods perform spatial inference queries by using **fuzzy spatial inference models**, which pursue the goal of discovering new meaningful findings from fuzzy spatial data. A spatial inference query combines spatial query processing with fuzzy inference methods to capture all points that intersect a given search object and whose output values fulfill some specific user requirements (e.g., the points with the maximum inferred values only). Here, we assume that the search object is a _window query_.

To conduct our comparative analysis, we consider the running example provided in the paper. Its goal is to recommend the locations in New York City that provide a **great** visiting experience, considering prices and overall ratings of accommodations and sanitary conditions of restaurants situated near to the location that the user wants to stay. These characteristics (i.e., prices, overall ratings, and sanitary conditions) are represented by fuzzy spatial data.

To implement the running example and, consequently, our comparative analysis, we have employed the [R package `fsr`](https://cran.r-project.org/package=fsr). Both region inference methods are available in this package.

# How to use?

An adapted version of this project is also publicly available in Code Ocean. In the version available in Code Ocean, we have decreased the number of configurations evaluated by the comparative analysis so that you can see how it works in a short, simple run. Furthermore, we have included the original results obtained in our comparative analysis; thus, you can generate the same figures as the figures included in the comparative analysis discussed in the paper.
