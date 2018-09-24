#version 330 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 SquareVertices;
layout(location = 1) in vec2 TextureCoords;

// Output data ; will be interpolated for each fragment.
out vec2 UV;

// Values that stay constant for the whole mesh.
uniform vec3 CameraRight_worldspace;
uniform vec3 CameraUp_worldspace;

uniform mat4 ProjMatrix;
uniform mat4 ViewMatrix;
uniform vec3 BillboardPos; // Position of the center of the billboard
uniform vec2 BillboardSize; // Size of the billboard, in world units (probably meters)

void main(void){
	vec3 particleCenter_worldspace = BillboardPos;
	vec3 vertexPosition_worldspace =
		  particleCenter_worldspace
		+ CameraRight_worldspace * SquareVertices.x * BillboardSize.x
		+ CameraUp_worldspace    * SquareVertices.y * BillboardSize.y;


	// Output position of the vertex
	gl_Position = ProjMatrix * ViewMatrix * vec4(vertexPosition_worldspace, 1.0f);

	// Or, if BillboardSize is in percentage of the screen size (1,1 for fullscreen) :
	//vertexPosition_worldspace = particleCenter_wordspace;
	//gl_Position = VP * vec4(vertexPosition_worldspace, 1.0f); // Get the screen-space position of the particle's center
	//gl_Position /= gl_Position.w; // Here we have to do the perspective division ourselves.
	//gl_Position.xy += SquareVertices.xy * vec2(0.2, 0.05); // Move the vertex in directly screen space. No need for CameraUp/Right_worlspace here.

	// Or, if BillboardSize is in pixels :
	// Same thing, just use (ScreenSizeInPixels / BillboardSizeInPixels) instead of BillboardSizeInScreenPercentage.

	// UV of the vertex. No special space for this one.
  UV = TextureCoords;
	// UV = SquareVertices.xy + vec2(0.5, 0.5);
}
