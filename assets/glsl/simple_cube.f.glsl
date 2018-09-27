#version 330 core

in vec3 BoxFragColor;

out vec3 color;

void main(void) {
	// color = vec3(0.702f, 0.729f, 0.655f);
	color = BoxFragColor;
}
