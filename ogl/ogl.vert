#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vColor;

uniform mat4 MVP;
varying vec4 f_color;

void main()
{
	gl_Position = MVP * vPosition;
	f_color = vec4(1.0, 1.0, 1.0, 1.0);
	//f_color = vec4(vPosition.x, vPosition.y, vPosition.z, 1.0);
}

