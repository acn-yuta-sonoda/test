import re, math, random # regexes, math functions, random numbers
import matplotlib.pyplot as plt # pyplot
from collections import defaultdict, Counter
from functools import partial, reduce

#
# functions for working with vectors
#
def vector_add(v, w):
    return [v_i + w_i for v_i, w_i in zip(v, w)]

def vector_subtract(v, w):
    return [v_i - w_i for v_i, w_i in zip(v, w)]

def vector_sum(vectors):
    result = vectors[0]
    for vector in vectors[1:]:
        result = vector_add(result, vector)
    return result

def scalar_multiply(c, v):
    return [c * v_i for v_i in v]

def vector_mean(vectors):
    n = len(vectors)
    return scalar_multiply(1/n, vector_sum(vectors))

def dot(v, w):
    return sum(v_i * w_i for v_i, w_i in zip(v, w))

def sum_of_squares(v):
    return dot(v, v)

def magnitude(v):
    return math.sqrt(sum_of_squares(v))

def squared_distance(v, w):
    return sum_of_squares(vector_subtract(v, w))

def distance(v, w):
    return math.sqrt(squared_distance(v, w))

#
# functions for working with matrices
#
def shape(A):
    num_rows = len(A)
    num_cols = len(A[0]) if A else 0
    return num_rows, num_cols

def get_row(A, i):
    return A[i-1]

def get_column(A, j):
    return [A_i[j-1] for A_i in A]

def make_matrix(num_rows, num_cols, entry_fn):
    return [[entry_fn(i, j)
             for j in range(num_cols)]
            for i in range(num_rows)]

def is_diagnoal(i, j):
    return 1 if i == j else 0

def identity_matrix(n):
    def is_diagnoal(i, j):
        return 1 if i == j else 0
    
    return [[is_diagnoal(i, j)
             for j in range(n)]
            for i in range(n)]

def matrix_add(A, B):
    if shape(A) != shape(B):
        return None
    else:
        def vector_add(v, w):
            return [v_i + w_i for v_i, w_i in zip(v, w)]
        
        return [vector_add(a_i, b_i) for a_i, b_i in zip(A, B)]

def matrix_subtract(A, B):
    if shape(A) != shape(B):
        return None
    else:
        def vector_subtract(v, w):
            return [v_i - w_i for v_i, w_i in zip(v, w)]
        
        return [vector_subtract(a_i, b_i) for a_i, b_i in zip(A, B)]

def matrix_multiply(A, B):
    cols_A = len(A[0]) if A else 0
    rows_B = len(B)
    
    if cols_A != rows_B:
        print("Cannot multiply A with B")
    else:
        num_cols = len(B[0]) if B else 0
        num_rows = len(A)
        
        def is_element(i, j):
            element = 0
            for k in range(cols_A):
                element = element + A[i][k] * B[k][j]
            return element
        
        return [[is_element(i, j)
                 for j in range(num_cols)]
                for i in range(num_rows)]
