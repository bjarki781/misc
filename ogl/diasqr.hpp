// maybe a light wrapper, .area() is useful
template <typename T>
using Matrix = std::vector<std::vector<T>>;

template <typename T>
size_t vecsib(const typename std::vector<T>& vec)
{
    return sizeof(T) * vec.size();
}

int ipow(int base, int exp)
{
	int result = 1;
	while (exp)
	{
		if (exp & 1)
			result *= base;
		exp >>= 1;
		base *= base;
	}

	return result;
}

bool even(int n) 
{
	return n % 2 == 0;
}

// clamped random float
float clrand()
{ 
	static std::random_device rd;
	static std::mt19937 gen(rd());
	std::uniform_real_distribution<float> dis(-1, 1);
	//VAR(dis(gen));
	return dis(gen);
}

float avg(float n1, float n2, float n3)
{
	return (n1 + n2 + n3) / 3.0;
}

float avg(float n1, float n2, float n3, float n4)
{
	return (n1 + n2 + n3 + n4) / 4.0;
}

void dia(Matrix<float> &mat, int d)
{
	float v;
	int size = mat[0].size()-1;
	int stride = size / d;

	//VAR(size);

	VAR(d);
	for (int i = 0; i+stride <= size; i += stride) {
		//		VAR(i);
		for (int j = 0; j+stride <= size; j += stride) {
			//			VAR(j);
			v = avg(mat[i][j],
					mat[i][j+stride], 
					mat[j+stride][j], 
					mat[j+stride][j+stride]) + clrand()/d;
			mat[i+stride/2][j+stride/2] = v;
		}
	}

}

void sqr(Matrix<float> &mat, int d)
{
	float v;
	int size = mat[0].size()-1;
	int stride = size / d;

	//VAR(size);

	for (int i = 0; i <= size; i += stride/2) {
		//		VAR(i);
		for (int j = (even(i/(stride/2)) ? stride/2 : 0); j <= size; j += stride) {
			//			VAR(j);

			if (j == 0)
				v = avg(mat[i-stride/2][0], 
						mat[i+stride/2][0], 
						mat[i][0+stride/2]) + clrand()/d;
			else if (i == 0)
				v = avg(mat[0][j-stride/2], 
						mat[0][j+stride/2], 
						mat[0+stride/2][j]) + clrand()/d;
			else if (j == size)
				v = avg(mat[i-stride/2][size], 
						mat[i+stride/2][size], 
						mat[i][size-stride/2]) + clrand()/d;
			else if (i == size)
				v = avg(mat[size][j-stride/2], 
					mat[size][j+stride/2], 
					mat[size-stride/2][j]) + clrand()/d;
			else
				v = avg(mat[i][j-stride/2], 
					mat[i][j+stride/2], 
					mat[i-stride/2][j], 
					mat[i+stride/2][j]) + clrand()/d;
			mat[i][j] = v;
		}
	}
}

Matrix<float> generate_diamondsqr(int size)
{
	Matrix<float> matrix(size);
	for (int i = 0; i < size; i++)
		matrix[i] = std::vector<float>(size, 7.0f);
	
	matrix[0][0] = 0.0f;
	matrix[0][size-1] = 0.0f;
	matrix[size-1][0] = 0.0f;
	matrix[size-1][size-1] = 0.0f;

	//matrix[0][size/2]    = clrand();
	//matrix[size/2][0]    = clrand();
	//matrix[size/2][size-1] = clrand();
	//matrix[size-1][size/2] = clrand();

	for (int d = 1; d < size-1; d = d << 1) {
		dia(matrix, d);
		sqr(matrix, d);
	}

	return matrix;
}

