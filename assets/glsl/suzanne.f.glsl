#version 330 core

in vec4 frontColor; // color for front face
in vec4 backColor; // color for back face

out vec4 color;

void main(void) {
	// is the fragment part of a front face?
  if (gl_FrontFacing) color = frontColor;
	// fragment is part of a back face
  else color = backColor;
}
