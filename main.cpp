#include <string>
#include <cmath>
#include <vector>
#include <iostream>
using namespace std;
using ll = long long;
using ull = unsigned long long;
using db = double;
using uint = unsigned int;


class Matrix {
protected:
    int n;
    int m;
    unique_ptr<unique_ptr<double[]>[]> matrix;
    friend ostream& operator<<(ostream& os, const Matrix& matrix) {
        string output = "";
        for (int i = 0; i < matrix.n; i++) {
            for (int j = 0; j < matrix.m; j++) {
                string num = to_string(round(matrix.matrix[i][j] * matrix.roundValue) / matrix.roundValue);
                for (int k = 0; k < num.size(); k++) {
                    output += num[k];
                    if (num[k] == '.') {
                        for (int f = 0; f < log10(matrix.roundValue); f++) {
                            output += num[k + f + 1];
                        }
                        break;
                    }
                }
                if (j < matrix.m - 1) {
                    output += " ";
                }
            }
            output += "\n";
        }
        return os << output;
    }
    friend istream& operator>>(istream& is, const Matrix& matrix) {
        for (int i = 0; i < matrix.n; i++) {
            for (int j = 0; j < matrix.m; j++) {
                is >> matrix.matrix[i][j];
            }
        }
        return is;
    }
public:
    int roundValue = 1e6;
    Matrix operator+(const Matrix& matrix) const {
        if (this->n == matrix.n and this->m == matrix.m) {
            Matrix res(this->n, this->m);
            for (int i = 0; i < this->n; i++) {
                for (int j = 0; j < this->m; j++) {
                    res.matrix[i][j] = this->matrix[i][j] + matrix.matrix[i][j];
                }
            }
            return res;
        }
        else {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
    }
    Matrix* operator+(const Matrix* matrix) const {
        return new Matrix(*this + *matrix);
    }
    Matrix operator-(const Matrix& matrix) const {
        if (this->n == matrix.n and this->m == matrix.m) {
            Matrix res(this->n, this->m);
            for (int i = 0; i < this->n; i++) {
                for (int j = 0; j < this->m; j++) {
                    res.matrix[i][j] = this->matrix[i][j] - matrix.matrix[i][j];
                }
            }
            return res;
        }
        else {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
    }
    Matrix* operator-(Matrix* matrix) const {
        return new Matrix(*this - *matrix);
    }
    Matrix operator*(const Matrix& matrix) const {
        if (this->m == matrix.n) {
            Matrix res(this->n, matrix.m);
            for (int i = 0; i < this->n; i++) {
                for (int j = 0; j < matrix.m; j++) {
                    for (int k = 0; k < this->m; k++) {
                        res.matrix[i][j] += this->matrix[i][k] * matrix.matrix[k][j];
                    }
                }
            }
            return res;
        }
        else {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
    }
    Matrix operator*(const double& constant) {
        Matrix res(this->n, this->m);
        for (int i = 0; i < this->n; i++) {
            for (int j = 0; j < this->m; j++) {
                res.matrix[i][j] = this->matrix[i][j] * constant;
            }
        }
        return res;
    }
    Matrix* operator*(const Matrix* matrix) const {
        return new Matrix(*this * *matrix);
    }
    Matrix operator=(const Matrix& matrix) {
        this->n = matrix.n;
        this->m = matrix.m;
        this->matrix.release();
        this->matrix = unique_ptr<unique_ptr<double[]>[]>(new unique_ptr<double[]>[this->n]);
        for (int i = 0; i < matrix.n; i++) {
            this->matrix[i] = unique_ptr<double[]>(new double[this->m]);
            for (int j = 0; j < matrix.m; j++) {
                this->matrix[i][j] = matrix.matrix[i][j];
            }
        }
        return *this;
    }
    Matrix* operator=(const Matrix* matrix) {
        *this = *matrix;
        return this;
    }
    Matrix transpose() const {
        Matrix res(this->m, this->n);
        for (int i = 0; i < this->n; i++) {
            for (int j = 0; j < this->m; j++) {
                res.matrix[j][i] = this->matrix[i][j];
            }
        }
        return res;
    }
    int getN() const {
        return this->n;
    }
    int getM() const {
        return this->m;
    }
    double getElem(int i, int j) const {
        if (i >= this->n or j >= this->m) {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
        return this->matrix[i][j];
    }
    Matrix() = delete;
    Matrix(int n, int m) {
        this->n = n;
        this->m = m;
        matrix = unique_ptr<unique_ptr<double[]>[]>(new unique_ptr<double[]>[n]);
        for (int i = 0; i < n; i++) {
            matrix[i] = unique_ptr<double[]>(new double[m]);
            for (int j = 0; j < m; j++) {
                matrix[i][j] = 0;
            }
        }
    }
    Matrix(const Matrix& matrix) {
        this->n = matrix.n;
        this->m = matrix.m;
        this->matrix = unique_ptr<unique_ptr<double[]>[]>(new unique_ptr<double[]>[this->n]);
        for (int i = 0; i < matrix.n; i++) {
            this->matrix[i] = unique_ptr<double[]>(new double[this->m]);
            for (int j = 0; j < matrix.m; j++) {
                this->matrix[i][j] = matrix.matrix[i][j];
            }
        }
    }
};


class SquareMatrix : public Matrix {
public:
    Matrix operator+(const Matrix& matrix) {
        Matrix first(*this);
        return first + matrix;
    }
    Matrix operator-(const Matrix& matrix) {
        Matrix first(*this);
        return first - matrix;
    }
    Matrix operator*(const Matrix& matrix) {
        Matrix first(*this);
        return first * matrix;
    }
    Matrix* operator+(Matrix* matrix) {
        return (SquareMatrix*)(*(Matrix*)this + matrix);
    }
    Matrix* operator-(Matrix* matrix) {
        return (SquareMatrix*)(*(Matrix*)this - matrix);
    }
    Matrix* operator*(Matrix* matrix) {
        return (SquareMatrix*)(*(Matrix*)this * matrix);
    }
    Matrix operator=(const Matrix& matrix) {
        if (matrix.getN() != matrix.getM()) {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
        this->n = matrix.getN();
        this->m = matrix.getM();
        this->matrix.release();
        this->matrix = unique_ptr<unique_ptr<double[]>[]>(new unique_ptr<double[]>[this->n]);
        for (int i = 0; i < matrix.getN(); i++) {
            this->matrix[i] = unique_ptr<double[]>(new double[this->m]);
            for (int j = 0; j < matrix.getM(); j++) {
                this->matrix[i][j] = matrix.getElem(i, j);
            }
        }
        return *this;
    }
    SquareMatrix() = delete;
    SquareMatrix(int n) : Matrix(n, n) {};
    SquareMatrix(const Matrix& matrix) : Matrix(matrix) {
        if (matrix.getN() != matrix.getM()) {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
    }
};


class IdentityMatrix : public SquareMatrix {
public:
    IdentityMatrix() = delete;
    IdentityMatrix(int n) : SquareMatrix(n) {
        for (int i = 0; i < n; i++) {
            this->matrix[i][i] = 1;
        }
    }
};


class PermutationMatrix : public SquareMatrix {
public:
    PermutationMatrix() = delete;
    PermutationMatrix(const Matrix& matrix, int v, int u) : SquareMatrix(matrix.getN()) {
        if (v > this->n or u > this->m) {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
        for (int i = 0; i < this->n; i++) {
            if (i == v - 1) {
                this->matrix[i][u - 1] = 1;
            }
            else if (i == u - 1) {
                this->matrix[i][v - 1] = 1;
            }
            else {
                this->matrix[i][i] = 1;
            }
        }
    }
};


class EliminationMatrix : public SquareMatrix {
public:
    EliminationMatrix() = delete;
    EliminationMatrix(const Matrix& matrix, int e_v, int e_u, int p_v, int p_u) : SquareMatrix(matrix.getN()) {
        if (e_v > this->n or e_u > matrix.getM() or p_v > this->n or p_u > matrix.getM()) {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
        for (int i = 0; i < this->n; i++) {
            this->matrix[i][i] = 1;
            if (i == e_v - 1) {
                this->matrix[i][p_v - 1] = -matrix.getElem(i, e_u - 1) / matrix.getElem(p_v - 1, p_u - 1);
            }
        }
    }
};


class NormalizationMatrix : public SquareMatrix {
public:
    NormalizationMatrix() = delete;
    NormalizationMatrix(const Matrix& matrix, int row) : SquareMatrix(matrix.getN()) {
        for (int i = 0; i < n; i++) {
            this->matrix[i][i] = 1;
            if (row - 1 == i) {
                this->matrix[i][i] = 1 / matrix.getElem(i, i);
            }
        }
    }
};


class ColumnVector : public Matrix {
public:
    double norm() {
        double res = 0;
        for (int i = 0; i < this->n; i++) {
            res += pow(this->matrix[i][0], 2);
        }
        return sqrt(res);
    }
    ColumnVector() = delete;
    ColumnVector(int n) : Matrix(n, 1) { }
    ColumnVector(int n, double val) : Matrix(n, 1) {
        for (int i = 0; i < n; i++) {
            this->matrix[i][0] = val;
        }
    }
    ColumnVector(const Matrix& matrix) : Matrix(matrix.getN(), 1) {
        if (matrix.getM() != 1) {
            throw invalid_argument("Error: the dimensional problem occurred\n");
        }
        for (int i = 0; i < matrix.getN(); i++) {
            this->matrix[i][0] = matrix.getElem(i, 0);
        }
    }
    ColumnVector(const vector<double>& input) : Matrix(input.size(), 1) {
        for (int i = 0; i < input.size(); i++) {
            this->matrix[i][0] = input[i];
        }
    }
};


class DiagonalMatrix : public Matrix {
public:
    DiagonalMatrix() = delete;
    DiagonalMatrix(const ColumnVector& init) : Matrix(init.getN(), init.getN()) {
        for (int i = 0; i < this->n; i++) {
            this->matrix[i][i] = init.getElem(i, 0);
        }
    }
};


// Function that makes an upper triangular matrix from the given one
SquareMatrix makeUpperTrinagular(Matrix& matrix, Matrix& e, int& steps) {
    int pivots = 0;
    for (int j = 0; j < matrix.getM(); j++) {
        if (pivots >= matrix.getN() - 1) {
            break;
        }
        double maxVal = abs(matrix.getElem(pivots, j));
        int row = pivots;
        for (int i = pivots + 1; i < matrix.getN(); i++) {
            if (abs(matrix.getElem(i, j)) > maxVal) {
                maxVal = abs(matrix.getElem(i, j));
                row = i;
            }
        }
        if (maxVal == 0) {
            continue;
        }
        if (row != pivots) {
            PermutationMatrix tmpMatrix(matrix, pivots + 1, row + 1);
            matrix = tmpMatrix * matrix;
            e = tmpMatrix * e;
        }
        for (int i = pivots + 1; i < matrix.getN(); i++) {
            if (matrix.getElem(i, j) != 0) {
                EliminationMatrix tmpMatrix(matrix, i + 1, j + 1, pivots + 1, j + 1);
                matrix = tmpMatrix * matrix;
                e = tmpMatrix * e;
            }
        }
        pivots++;
    }
    return e;
}


// Function that makes a down triangular matrix from the given one
SquareMatrix makeDownTrinagular(Matrix& matrix, Matrix& e, int& steps, bool second) {
    int pivots = 0;
    for (int j = matrix.getM() - 1; j > 0; j--) {
        if (pivots >= matrix.getN() - 1) {
            break;
        }
        if (!second) {
            double maxVal = abs(matrix.getElem(matrix.getN() - pivots - 1, j));
            int row = matrix.getN() - pivots - 1;
            for (int i = matrix.getN() - pivots - 2; i >= 0; i--) {
                if (abs(matrix.getElem(i, j)) > maxVal) {
                    maxVal = abs(matrix.getElem(i, j));
                    row = i;
                }
            }
            if (maxVal == 0) {
                continue;
            }
            if (row != matrix.getN() - pivots - 1) {
                PermutationMatrix tmpMatrix(matrix, matrix.getN() - pivots, row + 1);
                matrix = tmpMatrix * matrix;
                e = tmpMatrix * e;
            }
        }
        for (int i = matrix.getN() - pivots - 2; i >= 0; i--) {
            if (matrix.getElem(i, j) != 0) {
                EliminationMatrix tmpMatrix(matrix, i + 1, j + 1, matrix.getN() - pivots, j + 1);
                matrix = tmpMatrix * matrix;
                e = tmpMatrix * e;
            }
        }
        pivots++;
    }
    return e;
}


// Function that checks if the matrix is singular
bool isSingular(SquareMatrix matrix) {
    int c = 0;
    SquareMatrix tmp(IdentityMatrix(matrix.getN()));
    makeUpperTrinagular(matrix, tmp, c);
    for (int i = 0; i < matrix.getN(); i++) {
        if (matrix.getElem(i, i) == 0) {
            return true;
        }
    }
    return false;
}


// Function that normalizes a diagonal matrix
SquareMatrix diagonalNormalization(SquareMatrix& matrix, Matrix& e) {
    for (int i = 0; i < matrix.getN(); i++) {
        if (matrix.getElem(i, i) == 0) {
            continue;
        }
        NormalizationMatrix tmpMatrix(matrix, i + 1);
        matrix = tmpMatrix * matrix;
        e = tmpMatrix * e;
    }
    return e;
}


// Function that calculates inverse matrix
SquareMatrix findInverse(SquareMatrix matrix) {
    int c = 0;
    if (isSingular(matrix)) {
        throw invalid_argument("Error: matrix A is singular\n");
    }
    SquareMatrix e(IdentityMatrix(matrix.getN()));
    makeUpperTrinagular(matrix, e, c);
    makeDownTrinagular(matrix, e, c, true);
    diagonalNormalization(matrix, e);
    return e;
}


// Function that checks if the solution vaild for given constraints
bool checkValidity(const ColumnVector& solution, const Matrix& a, const ColumnVector& b) {
    ColumnVector constraints = a * solution;
    for (int i = 0; i < b.getN(); i++) {
        if (constraints.getElem(i, 0) > b.getElem(i, 0)) {
            return false;
        }
    }
    return true;
}


// Function that calculates the projection of aPrime in the formula for p
Matrix findProjection(const Matrix& a) {
    Matrix aTranspose = a.transpose();
    return aTranspose * findInverse(a * aTranspose) * a;
}


// Interior point algorithm function
ColumnVector interiorPoint(ColumnVector solution, Matrix a, ColumnVector b, Matrix c, double alpha, double epsilon) {
    ColumnVector prevSolution(solution.getN(), INFINITY);
    // Check that the given solution is valid
    if (!checkValidity(solution, a, b)) {
        throw invalid_argument("Error: solution is not valid\n");
    }
    // Iterate the algorithm while the solution is not accurate enough
    while (abs(solution.norm() - prevSolution.norm()) > epsilon) {
        double beta = -1;
        DiagonalMatrix d(solution);
        Matrix aPrime = a * d;
        ColumnVector cPrime = d * c;
        Matrix p = IdentityMatrix(solution.getN()) - findProjection(aPrime);
        ColumnVector cp = p * cPrime;
        for (int i = 0; i < cp.getN(); i++) {
            if (cp.getElem(i, 0) < 0 && abs(cp.getElem(i, 0)) > beta) {
                beta = abs(cp.getElem(i, 0));
            }
        }
        ColumnVector xPrime = ColumnVector(solution.getN(), 1) + (cp * (alpha / beta));
        prevSolution = solution;
        solution = d * xPrime;
    }
    return solution;
}


/*
Input example:
2 4
1 1 0 0
2 4 1 0
1 3 0 -1
16 9
0.5 3.5 1 2
0.5
0.000001
*/


int main() {
    try {
        cout << "Input the number of constraints and number of variables in objective function:\n";
        int constraintsNumber, variablesNumber;
        cin >> constraintsNumber >> variablesNumber;
        cout << "Input objective function coefficients:\n";
        Matrix cInput(1, variablesNumber);
        cin >> cInput;
        ColumnVector c = cInput.transpose();
        cout << "Input the matrix of coefficients of constraint function:\n";
        Matrix a(constraintsNumber, variablesNumber);
        cin >> a;
        cout << "Input constraints RHS numbers:\n";
        Matrix bInput(1, constraintsNumber);
        cin >> bInput;
        ColumnVector b = bInput.transpose();
        cout << "Input the first feasible solution:\n";
        Matrix xInput(1, variablesNumber);
        cin >> xInput;
        ColumnVector x = xInput.transpose();
        cout << "Input the Alpha:\n";
        double alpha;
        cin >> alpha;
        cout << "Input the solution precision:\n";
        double epsilon;
        cin >> epsilon;
        x = interiorPoint(x, a, b, c, alpha, epsilon);
        x.roundValue = pow(10, abs(log10(epsilon)));
        cout << "Result solution vector:\n" << x.transpose();
        cout << "Numerical result:\n";
        cout << x.transpose() * c;
    }
    catch (const exception& ex) {
        cout << ex.what() << "\n";
    }
}
