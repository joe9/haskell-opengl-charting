
#define WANT_ARCAN_SHMIF_HELPER
#define GL_GLEXT_PROTOTYPES

#include <stdlib.h>
#include <unistd.h>
#include <EGL/egl.h>
#include <GL/gl.h>
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
	    /* programming error; will fail on all hardware */		\
	    fprintf(stderr,"CHECK_FRAMEBUFFER_STATUS: unknown error\n"); \
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

int main(int argc, char ** argv)
{
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

   printf("headless graphics setup complete\n");
   char *response = (char *)eglQueryString(EGL_NO_DISPLAY,EGL_CLIENT_APIS);
   printf("EGL_CLIENT_APIS: %s\n",response);
   response = (char *)eglQueryString(EGL_NO_DISPLAY,EGL_VENDOR);
   printf("EGL_VENDOR: %s\n",response);
   response = (char *)eglQueryString(EGL_NO_DISPLAY,EGL_VERSION);
   printf("EGL_VERSION: %s\n",response);
   response = (char *)eglQueryString(EGL_NO_DISPLAY,EGL_EXTENSIONS);
   printf("EGL_EXTENSIONS: %s\n",response);

   response = (char *)glGetString(GL_VENDOR);
   printf("GL_VENDOR: %s\n",response);
   response = (char *)glGetString(GL_RENDERER);
   printf("GL_VENDOR: %s\n",response);
   response = (char *)glGetString(GL_VERSION);
   printf("GL_VERSION: %s\n",response);
   response = (char *)glGetString(GL_EXTENSIONS);
   printf("GL_EXTENSIONS: %s\n",response);

   printf("headless graphics generating framebuffer\n");
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

   glBindTexture(GL_TEXTURE_2D, 0);
   glEnable(GL_TEXTURE_2D);
   glBindFramebuffer(GL_FRAMEBUFFER, framebufferName);

   /* clear the color buffer */
   glClearColor(1.0, 1.0, 0.0, 1.0);
   glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
   glBindFramebuffer(GL_FRAMEBUFFER, 0);
   GLenum ErrorCheckValue = glGetError();
   if (ErrorCheckValue != GL_NO_ERROR)
   {
      fprintf(stderr, "ERROR: Could not draw the triangles: %i \n", ErrorCheckValue);
      exit(-1);
   }

   printf("headless graphics before sending eglsignal\n");
   /* assuming that this call should always work */
   /*    glFlush(); */

   printf("headless graphics after glFlush\n");
   /*    need to render from the texture */
   if (arcan_shmifext_eglsignal(&arcanShmifControl,
				0,
				SHMIF_SIGVID, colorTextureName) >= 0){
      printf("headless graphics sleeping\n");
      return 0;
   }

   /*    eglSwapBuffers(display, surface); */
   printf("headless graphics sleeping\n");

   int i =0;
   while (i<1000){
      i++;
      sleep(60);
   }
   sleep(60);
   sleep(60);

   printf("headless graphics after sleeping\n");
   /*    is this good enough for freeing the above created resources */
   arcan_shmif_drop(&arcanShmifControl);

   glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, 0);
   glBindRenderbuffer(GL_RENDERBUFFER, 0);
   glDeleteRenderbuffers(1, &depthRenderBufferName);

   glBindFramebuffer(GL_FRAMEBUFFER,0);
   glDeleteFramebuffers(1,&framebufferName);
   return EXIT_SUCCESS;
}

