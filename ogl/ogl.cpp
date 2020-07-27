#include <iostream>
#include <cstring>
#include <cstdlib>
#include <cmath>
#include <ctime>
#include <algorithm>
#include <vector>
#include <random>

#define GLM_FORCE_RADIANS
#define _DEBUG

#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <GL/glu.h>
#include <GL/gl.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtc/constants.hpp>
#include <glm/gtx/string_cast.hpp>

#define BUFFER_OFFSET(n) ((void*)n)
#define VAR(v) (std::cout << __func__ << ": " #v " = " << v << std::endl)

#include "loadshaders.hpp"
#include "opengl-debug.hpp"
#include "callbacks.hpp"
#include "diasqr.hpp"

enum VAO_ids {Triangles, lenVAOs};
enum Buffer_ids {ArrayBuffer, lenBufs};
enum Attrib_ids {vPosition, vColor};

GLuint VAOs[lenVAOs];
GLuint Buffers[lenBufs];

enum Axis {X, Y, Z};

int indices_per_vertex = 0;

GLFWwindow *init_win(const int width, const int heigth, std::string win_name)
{
	GLFWwindow *window;

	glfwSetErrorCallback(error_callback);

	if (!glfwInit()) exit(1);

	window = glfwCreateWindow(width, heigth, win_name.data(), NULL, NULL);
	if (!window) {
		glfwTerminate();
		exit(1);
	}

	glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);
	glfwSetKeyCallback(window, key_callback);

	glfwMakeContextCurrent(window);

	glewExperimental = true;
	if (glewInit() != GLEW_OK) {
		std::cerr << "Glew failed to initialize..." << std::endl;
		exit(1);
	}

	GLint flags;
	glGetIntegerv(GL_CONTEXT_FLAGS, &flags);

	glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, GL_TRUE);
	if (flags & GL_CONTEXT_FLAG_DEBUG_BIT) {
		;
	}

	return window;
}

std::vector<float> from_vecmatrix(Matrix<glm::vec3> vecmatrix)
{
	std::vector<float> vec(vecmatrix.size() * vecmatrix.size());

	for (int i = 0; i < vecmatrix.size(); i++) {
		for (int j = 0, k = 0; j < vecmatrix.size(); j++, k += 3) {
			vec[k+0] = vecmatrix[i][j].x;
			vec[k+1] = vecmatrix[i][j].y;
			vec[k+2] = vecmatrix[i][j].z;
		}
	}

	return vec;
}

float range(float lo, float hi, int steps, int curstep)
{
	float range = abs(hi - lo);
	return lo + range/steps*curstep;
}

int flatten(int row_len, int x, int y)
{
	return x*row_len+y;
}

GLint init_gl(const bool multisampling, Matrix<float> ymatrix)
{
	GLint program;

	glEnable(GL_PRIMITIVE_RESTART);
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_DEBUG_OUTPUT);
	glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
	glDebugMessageCallback(debugMessage, nullptr); 
	glDebugMessageControl(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, nullptr, GL_TRUE);

	glPointSize(2.0);

    const GLubyte *ver = glGetString(GL_VERSION);
    std::cout << "Hello" << std::endl;
    std::cout << ver << std::endl;


	if (multisampling) {
		int v;

		glGetIntegerv(GL_SAMPLE_BUFFERS, &v);

		if (v == 1) {
			glEnable(GL_MULTISAMPLE);
			glEnable(GL_SAMPLE_SHADING);
			glEnable(GL_LINE_SMOOTH); 
			glGetIntegerv(GL_SAMPLES, &v);
			std::cout << "using " << v
				  << "x multisampling" << std::endl;
		} else {
			std::cerr << "multisampling could not be enabled..." 
				  << std::endl;
		}
	}
	
	Matrix<glm::vec3> vecmatrix(ymatrix.size());
	for (int i = 0; i < ymatrix.size(); i++)
		vecmatrix[i] = std::vector<glm::vec3>(ymatrix.size());

	for (int i = 0; i < ymatrix.size(); i++) {
		for (int j = 0; j < ymatrix.size(); j++) {
			vecmatrix[i][j].x = range(-1.0f, 1.0f, ymatrix.size()-1, i);
			//vecmatrix[i][j].y = ymatrix[i][j];
			vecmatrix[i][j].y = 0.0f;
			vecmatrix[i][j].z = range(-1.0f, 1.0f, ymatrix.size()-1, j);
			printf("%3.1f, %3.1f, %3.1f\n", vecmatrix[i][j].x, vecmatrix[i][j].y, vecmatrix[i][j].z);
		}
	}

	// provisional for some proper color calculating
	std::vector<float> colors(ymatrix.size()*ymatrix.size()*4);
	for (int j = 0; j < ymatrix.size()*ymatrix.size()*4; j += 4) {
		colors[j] = 0.5f;
		colors[j+1] = 1.0f/(float)j;
		colors[j+2] = 1.0f/(float)(j-1.0f);
		colors[j+3] = 1.0f;
	}

	// for each vertex there is a discrete triangle to render
	indices_per_vertex = 3;
	std::vector<unsigned int> indices(vecmatrix.size()*vecmatrix.size()*indices_per_vertex);  
	for (int i = 0, k = 0; i < vecmatrix.size() && k < indices.size()*vecmatrix.size()*indices_per_vertex; i++) {
		for (int j = 0; j < vecmatrix.size() && k < vecmatrix.size()*vecmatrix.size()*indices_per_vertex; j++, k += indices_per_vertex) {
			indices[k+0] = flatten(vecmatrix.size(), i, j);
			indices[k+1] = flatten(vecmatrix.size(), i+1, j);
			indices[k+2] = flatten(vecmatrix.size(), i, j+1);
		}
	}

	std::vector<float> flat = from_vecmatrix(vecmatrix);

	glGenVertexArrays(lenVAOs, VAOs);
	glBindVertexArray(VAOs[Triangles]);

	glGenBuffers(lenBufs, Buffers);
	glBindBuffer(GL_ARRAY_BUFFER, Buffers[ArrayBuffer]);
	glBufferData(GL_ARRAY_BUFFER, vecsib(flat) + vecsib(colors), NULL, GL_STATIC_DRAW);

	glBufferSubData(GL_ARRAY_BUFFER, 0, vecsib(flat), flat.data());
	glBufferSubData(GL_ARRAY_BUFFER, vecsib(flat), vecsib(colors), colors.data());

	glVertexAttribPointer(vPosition, 3, GL_FLOAT, GL_FALSE, 0, BUFFER_OFFSET(0));
	glVertexAttribPointer(vColor,    4, GL_FLOAT, GL_FALSE, 0, BUFFER_OFFSET(vecsib(flat)));
	glEnableVertexAttribArray(vPosition);
	glEnableVertexAttribArray(vColor);

	glPrimitiveRestartIndex(0xDEADBEEF);

	GLuint element_buffer;

	glGenBuffers(1, &element_buffer);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, element_buffer);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, vecsib(indices), indices.data(), GL_STATIC_DRAW);

	glClearColor(0.0, 0.0, 0.0, 1.0);
	glClearDepth(1.0);

	ShaderInfo shaders[] = {
		{GL_VERTEX_SHADER, "ogl.vert"},
		{GL_FRAGMENT_SHADER, "ogl.frag"},
		{GL_NONE, NULL},
	};

	program = LoadShaders(shaders);
	glUseProgram(program);

	return program;
}

void draw_mesh(int matrix_size)
{
//	glDrawElements(GL_POINTS, matrix_size*matrix_size*indices_per_vertex, GL_UNSIGNED_INT, BUFFER_OFFSET(0));
//	glDrawArrays(GL_TRIANGLE_STRIP, 0, vert_count);
	glDrawArrays(GL_POINTS, 0, matrix_size*matrix_size);
}

void display(const GLint program, glm::mat4 mvp, const int size)
{
	GLint mvp_loc;

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	mvp_loc = glGetUniformLocation(program, "MVP");
	assert(mvp_loc != -1);
	glProgramUniformMatrix4fv(program, mvp_loc, 1, GL_FALSE, 
			glm::value_ptr(mvp));

	glBindVertexArray(VAOs[Triangles]);
	draw_mesh(size*size*indices_per_vertex);
}

int main(int argc, char *argv[])
{
	GLFWwindow *window;
	GLint program;
	glm::mat4 project(1.0f), view(1.0f), model(1.0f);
	bool multisampling = true;


	srand(time(NULL));

	const int size = ipow(2, 3) + 1;

	Matrix<float> ymatrix = generate_diamondsqr(size);

	project = glm::perspective(glm::pi<float>()*0.2f, 640.0f/480.0f, 0.1f, 3.0f);
	model   = glm::scale(model, glm::vec3(0.2f));
	view    = glm::lookAt(glm::vec3(0.0f, 1.0f, 1.0f), 
			      glm::vec3(0.0f, 0.0f, 0.0f), 
			      glm::vec3(0.0f, 1.0f, 0.0f));

	window = init_win(640, 480, std::string("OGL"));
	program = init_gl(multisampling, ymatrix);

	while (!glfwWindowShouldClose(window)) {
		model = glm::rotate(model, 0.01f, glm::vec3(0.0f, 1.0f, 0.0f));
		display(program, project * view * model, size-1);

		glfwSwapBuffers(window);
		glfwPollEvents();
	}

	glfwDestroyWindow(window);
	glfwTerminate();

	exit(0);
}

