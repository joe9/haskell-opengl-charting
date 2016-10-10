
#define WANT_ARCAN_SHMIF_HELPER

/* #define EGL_EGLEXT_PROTOTYPES */
/* #define MESA_EGL_NO_X11_HEADERS */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>
#include <unistd.h>
#include <GL/glew.h>
#include <GLES2/gl2.h>
#include "shmif/arcan_shmif.h"
#include "shmif/shmif_privext.h"

#define CHECK_FRAMEBUFFER_STATUS()				\
   {								\
      GLenum status;						\
      status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);	\
      switch(status) {						\
	 case GL_FRAMEBUFFER_COMPLETE_EXT:			\
	    break;						\
	 case GL_FRAMEBUFFER_UNSUPPORTED_EXT:			\
	    /* choose different formats */			\
	    break;						\
	 default:						\
	    /* programming error; will fail on all hardware */	\
	    assert(0);						\
      }}

int CurrentWidth = 800,
   CurrentHeight = 600,
   WindowHandle = 0;
GLuint
colorTextureName, framebufferName, depthRenderBufferName,
   VertexShaderId,
   FragmentShaderId,
   ProgramId,
   VaoId,
   VboId,
   VaoId1,
   VboId1;

const GLchar* VertexShader =
{
   "#version 330\n"				\

   "layout(location=0) in vec2 inPosition;\n"	\

   "void main(void)\n"							\
   "{\n"								\
   "  gl_Position = vec4(inPosition.x,inPosition.y,0.0f,1.0f);\n"	\
   "}\n"
};
const GLchar* FragmentShader =
{
   "#version 330\n"				\

   "uniform vec4 color = vec4(1.0,0.0,0.0,1.0);\n"	\

   "out vec4 out_Color;\n"				\

   "void main(void)\n"							\
   "{\n"									\
   "  out_Color = color;\n"						\
   "}\n"
};
void CreateVBO(void)
{
   GLfloat dummyfloat = 0.1f;
   GLfloat Vertices[] = {
      -1, 0
      ,-0.5, 1
      ,0, 0

      ,0, 0
      , 0.5, 1
      , 1, 0

      , 0.5, 0
      , 1, -1
      , 0, -1
   };

      GLenum ErrorCheckValue = glGetError();

      glGenVertexArrays(1, &VaoId);
      glBindVertexArray(VaoId);

      glGenBuffers(1, &VboId);
      glBindBuffer(GL_ARRAY_BUFFER, VboId);
      /*    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0); */

      glBindVertexBuffer(0, VboId, 0, 2 * sizeof(dummyfloat));
      glVertexAttribFormat(0, 2, GL_FLOAT, GL_FALSE,0);
      glVertexAttribBinding(0, 0);
      glEnableVertexAttribArray(0);

      ErrorCheckValue = glGetError();
      if (ErrorCheckValue != GL_NO_ERROR)
      {
	 fprintf(
	     stderr,
	     "ERROR: Could not create a VBO: %i \n", ErrorCheckValue
		 );

	 exit(-1);
      }

      glGenVertexArrays(1, &VaoId1);
      glBindVertexArray(VaoId1);

      glGenBuffers(1, &VboId1);
      glBindBuffer(GL_ARRAY_BUFFER, VboId1);
      /*    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0); */

      glBindVertexBuffer(0, VboId1, 0, 2 * sizeof(dummyfloat));
      glVertexAttribFormat(0, 2, GL_FLOAT, GL_FALSE,0);
      glVertexAttribBinding(0, 0);
      glEnableVertexAttribArray(0);

      /*    the vertex array does not store the GL_ARRAY_BUFFER, hence,
       *    need the glBindBuffer before calling the glBufferData. The
       *    glBindBuffer call is not required for just draw'ing though. */
      /*    http://stackoverflow.com/a/21652930 */
      /*    It is also worth mentioning that the "current" binding for
       *    GL_ARRAY_BUFFER is not one of the states that VAOs
       *    track. While this binding is used by commands such as
       *    glVertexAttribPointer (...), VAOs do not store the binding
       *    they only store pointers (the GL_ARB_vertex_attrib_binding
       *    extension introduced alongside GL 4.3 complicates this a bit,
       *    so let us ignore it for simplicity). */
      /*    VAOs do remember what is bound to GL_ELEMENT_ARRAY_BUFFER,
       *    however, so that indexed drawing commands such as
       *    glDrawElements (...) function as you would expect (e.g. VAOs
       *    re-use the last element array buffer bound). */
      /*    glBindVertexArray(VaoId); */
      glBindBuffer(GL_ARRAY_BUFFER, VboId);
      glBufferData(GL_ARRAY_BUFFER, sizeof(Vertices), Vertices, GL_STREAM_DRAW);

      /*    glBindVertexArray(VaoId1); */
      glBindBuffer(GL_ARRAY_BUFFER, VboId1);
      glBufferData(GL_ARRAY_BUFFER, sizeof(Vertices), Vertices, GL_STREAM_DRAW);

      glBindVertexArray(VaoId);

      ErrorCheckValue = glGetError();
      if (ErrorCheckValue != GL_NO_ERROR)
      {
	 fprintf(stderr, "ERROR: Could not create a VBO: %i \n", ErrorCheckValue);

	 exit(-1);
      }
}
void DestroyVBO(void)
{
   GLenum ErrorCheckValue = glGetError();

   glDisableVertexAttribArray(1);
   glDisableVertexAttribArray(0);

   glBindBuffer(GL_ARRAY_BUFFER, 0);
   glDeleteBuffers(1, &VboId);

   glBindVertexArray(0);
   glDeleteVertexArrays(1, &VaoId);

   ErrorCheckValue = glGetError();
   if (ErrorCheckValue != GL_NO_ERROR)
   {
      fprintf(stderr, "ERROR: Could not destroy the VBO: %i \n", ErrorCheckValue);
	 exit(-1);
      }
   }
   void CreateShaders(void)
   {
      GLenum ErrorCheckValue = glGetError();
      GLfloat ColorUniform[] = {0.0f, 1.0f, 0.0f, 1.0f};

      VertexShaderId = glCreateShader(GL_VERTEX_SHADER);
      glShaderSource(VertexShaderId, 1, &VertexShader, NULL);
      glCompileShader(VertexShaderId);

      FragmentShaderId = glCreateShader(GL_FRAGMENT_SHADER);
      glShaderSource(FragmentShaderId, 1, &FragmentShader, NULL);
      glCompileShader(FragmentShaderId);

      ProgramId = glCreateProgram();
      glAttachShader(ProgramId, VertexShaderId);
      glAttachShader(ProgramId, FragmentShaderId);
      glLinkProgram(ProgramId);
      glUseProgram(ProgramId);

      ErrorCheckValue = glGetError();
      if (ErrorCheckValue != GL_NO_ERROR)
      {
	 fprintf(stderr, "ERROR: Could not create the shaders: %i \n", ErrorCheckValue);

	 exit(-1);
      }

      /*    http://www.lighthouse3d.com/tutorials/glsl-tutorial/color-example/ */
      GLint colorLocation = glGetUniformLocation(ProgramId, "color");
      glProgramUniform4fv(ProgramId, colorLocation, 1, ColorUniform);
   }
   void DestroyShaders(void)
   {
      GLenum ErrorCheckValue = glGetError();

      glUseProgram(0);

      glDetachShader(ProgramId, VertexShaderId);
      glDetachShader(ProgramId, FragmentShaderId);

      glDeleteShader(FragmentShaderId);
      glDeleteShader(VertexShaderId);

      glDeleteProgram(ProgramId);

      ErrorCheckValue = glGetError();
      if (ErrorCheckValue != GL_NO_ERROR)
      {
	 fprintf(stderr, "ERROR: Could not destroy the shaders: %i \n", ErrorCheckValue);
	 exit(-1);
      }
   }
   /* https://jan.newmarch.name/Wayland/ProgrammingClient/ */

int main(int argc, char **argv) {

   GLuint frameBufferName;
   GLuint colorRenderBufferName;

   /*    got the below arcan template from arcan/src/platform/arcan/video.c */
   static struct arg_arr* shmarguments;
   struct arcan_shmif_cont arcanShmifControl = arcan_shmif_open(
       SEGID_APPLICATION, SHMIF_ACQUIRE_FATALFAIL, &shmarguments);
   /*    if (!arcan_shmif_resize(&arcanShmifControl, 320, 200)) */
   /*       arcan_shmif_drop(&arcanShmifControl); */

   /*     TODO is it recommended to use this to get the arcan_shmif_cont instead of the open call above? arcan_shmif_primary(SHMIF_INPUT), */
   /* setup requires shmif connection as we might get metadata that way */
   enum shmifext_setup_status status;
   if ((status = arcan_shmifext_headless_setup(&arcanShmifControl,
					       arcan_shmifext_headless_defaults())) != SHMIFEXT_OK){
      printf("headless graphics setup failed, code: %d\n", status);
      arcan_shmif_drop(&arcanShmifControl);
      return false;
   }

   /*      if ((status = arcan_shmifext_headless_egl(&arcanShmifControl, */
   /*						&display, (void*) arcan_shmifext_headless_lookup, NULL)) != SHMIFEXT_OK){ */
   /*	 arcan_warning("headless graphics setup failed, code: %d\n", status); */
   /*	 arcan_shmif_drop(&arcanShmifControl); */
   /*	 return false; */
   /*       } */

   printf("connected using headless_setup\n");
   /*     TODO How do I get the frame buffer width and height at runtime - callback?  */

   /*  for sample code https://github.com/letoram/SDL2/blob/master/src/video/arcan/SDL_arcanopengl.c#L104 */
   glGenFramebuffers(1, &framebufferName);
   glGenTextures(1, &colorTextureName);
   glGenRenderbuffers(1, &depthRenderBufferName);

   glBindFramebuffer(GL_FRAMEBUFFER, framebufferName);

   glBindTexture(GL_TEXTURE_2D, colorTextureName);
   glTexImage2D(	GL_TEXTURE_2D,
			0,
			GL_RGBA,
			/* width, height */
			320, 200,
			0,
			GL_RGBA,
			GL_UNSIGNED_BYTE,
			NULL);

   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, colorTextureName, 0);

   glBindRenderbuffer(GL_RENDERBUFFER, depthRenderBufferName);
   glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, 320, 200);
   glFramebufferRenderbuffer(GL_DRAW_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depthRenderBufferName);

   CHECK_FRAMEBUFFER_STATUS();

   CreateShaders();
   CreateVBO();

   glBindTexture(GL_TEXTURE_2D, 0);
   glEnable(GL_TEXTURE_2D);
   glBindFramebuffer(GL_FRAMEBUFFER, framebufferName);

   glViewport(0,0, 320, 200);

   glClearColor(1,1,1,0);
   /*    glClearColor(0.0f, 0.0f, 0.0f, 0.0f); */
   glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );

   /* do not bother with depth testing or clearing the GL_DEPTH_BUFFER_BIT */
   glClear(GL_COLOR_BUFFER_BIT);
   /*    glDrawArrays(GL_TRIANGLE_STRIP, 0, 3); */
   /*    glDrawArrays(GL_LINES, 0, 6); */
   glDrawArrays(GL_TRIANGLES, 0, 9);

   glBindFramebuffer(GL_FRAMEBUFFER, 0);

   GLenum ErrorCheckValue = glGetError();
   if (ErrorCheckValue != GL_NO_ERROR)
   {
      fprintf(stderr, "ERROR: Could not draw the triangles: %i \n", ErrorCheckValue);
      exit(-1);
   }

   glFlush();
   /* but you need to explicitly synch, that is the arcan_shmif_signal or for GL, arcan_shmif_signalhandle */
   /* after rendering to the buffer, do arcan_shmifext_eglsignal to sync between arcan and teh connected client */
   /* from shmif/arcan_shmif_interop.h */
   /*    int arcan_shmifext_eglsignal(struct arcan_shmif_cont*, */
   /*				uintptr_t context, int mask, uintptr_t tex_id, ...); */
   /*    uintptr_t display; */
   /*    if (!arcan_shmifext_egl_meta(&arcanShmifControl,&display,NULL,NUll)) return 0; */
   /*    or, just pass 0 for display, the below function is looking it up from the control */
   if (arcan_shmifext_eglsignal(&arcanShmifControl,
				0,
				SHMIF_SIGVID, colorTextureName) >= 0)
      return 0;

   /*     I probably do not need this call? */
   /*     arcan_shmif_signal(&arcanShmifControl, SHMIF_SIGVID); */

   /*    sleep so I can see the results */
   sleep(120);
   arcan_shmif_drop(&arcanShmifControl);

   glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, 0);
   glBindRenderbuffer(GL_RENDERBUFFER, 0);
   glDeleteRenderbuffers(1, &colorRenderBufferName);

   glBindFramebuffer(GL_FRAMEBUFFER,0);
   glDeleteFramebuffers(1,&frameBufferName);

   DestroyShaders();
   DestroyVBO();

   printf("disconnected from display\n");
   exit(0);
}
