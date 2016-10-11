
#define WANT_ARCAN_SHMIF_HELPER
#define GL_GLEXT_PROTOTYPES

#include <stdlib.h>
#include <unistd.h>
#include <EGL/egl.h>
#include <GL/gl.h>
#include "shmif/arcan_shmif.h"
#include "shmif/shmif_privext.h"

#define CHECK_FRAMEBUFFER_STATUS()					\
   {									\
      GLenum status;							\
      status = glCheckFramebufferStatus(GL_FRAMEBUFFER);		\
      switch(status) {							\
	 case GL_FRAMEBUFFER_COMPLETE:					\
	    break;							\
	 case GL_FRAMEBUFFER_UNSUPPORTED:				\
	    /* choose different formats */				\
	    break;							\
	 default:							\
	    /* programming error; will fail on all hardware */		\
	    fprintf(stderr,"CHECK_FRAMEBUFFER_STATUS: unknown error\n"); \
      }}

// Return a description of the specified EGL error
//
static const char* getEGLErrorString(EGLint error)
{
   switch (error)
   {
      case EGL_SUCCESS:
	 return "Success";
      case EGL_NOT_INITIALIZED:
	 return "EGL is not or could not be initialized";
      case EGL_BAD_ACCESS:
	 return "EGL cannot access a requested resource";
      case EGL_BAD_ALLOC:
	 return "EGL failed to allocate resources for the requested operation";
      case EGL_BAD_ATTRIBUTE:
	 return "An unrecognized attribute or attribute value was passed in the attribute list";
      case EGL_BAD_CONTEXT:
	 return "An EGLContext argument does not name a valid EGL rendering context";
      case EGL_BAD_CONFIG:
	 return "An EGLConfig argument does not name a valid EGL frame buffer configuration";
      case EGL_BAD_CURRENT_SURFACE:
	 return "The current surface of the calling thread is a window, pixel buffer or pixmap that is no longer valid";
      case EGL_BAD_DISPLAY:
	 return "An EGLDisplay argument does not name a valid EGL display connection";
      case EGL_BAD_SURFACE:
	 return "An EGLSurface argument does not name a valid surface configured for GL rendering";
      case EGL_BAD_MATCH:
	 return "Arguments are inconsistent";
      case EGL_BAD_PARAMETER:
	 return "One or more argument values are invalid";
      case EGL_BAD_NATIVE_PIXMAP:
	 return "A NativePixmapType argument does not refer to a valid native pixmap";
      case EGL_BAD_NATIVE_WINDOW:
	 return "A NativeWindowType argument does not refer to a valid native window";
      case EGL_CONTEXT_LOST:
	 return "The application must destroy all contexts and reinitialise";
      default:
	 return "ERROR: UNKNOWN EGL ERROR";
   }
}

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

GLenum checkError_(const char *file, int line)
{
   GLenum errorCode;
   while ((errorCode = glGetError()) != GL_NO_ERROR)
   {
      char *error;
      switch (errorCode)
      {
	 case GL_INVALID_ENUM:                  error = "INVALID_ENUM"; break;
	 case GL_INVALID_VALUE:                 error = "INVALID_VALUE"; break;
	 case GL_INVALID_OPERATION:             error = "INVALID_OPERATION"; break;
	 case GL_STACK_OVERFLOW:                error = "STACK_OVERFLOW"; break;
	 case GL_STACK_UNDERFLOW:               error = "STACK_UNDERFLOW"; break;
	 case GL_OUT_OF_MEMORY:                 error = "OUT_OF_MEMORY"; break;
	 case GL_INVALID_FRAMEBUFFER_OPERATION: error = "INVALID_FRAMEBUFFER_OPERATION"; break;
      }
      printf("error %s, file %s, line %i\n",error, file, line);
   }
   return errorCode;
}
#define checkError() checkError_(__FILE__, __LINE__)


void APIENTRY debugOutput(GLenum source,
			  GLenum type,
			  GLuint id,
			  GLenum severity,
			  GLsizei length,
			  const GLchar *message,
			  void *userParam)
{
   // ignore non-significant error/warning codes
   if(id == 131169 || id == 131185 || id == 131218 || id == 131204) return;

   printf("---------------\n");
   printf("Debug message (%i): %s,length %i, userParam %s\n", id, message,length,(char *)userParam);

   switch (source)
   {
      case GL_DEBUG_SOURCE_API:             printf("Source: API"); break;
      case GL_DEBUG_SOURCE_WINDOW_SYSTEM:   printf("Source: Window System"); break;
      case GL_DEBUG_SOURCE_SHADER_COMPILER: printf("Source: Shader Compiler"); break;
      case GL_DEBUG_SOURCE_THIRD_PARTY:     printf("Source: Third Party"); break;
      case GL_DEBUG_SOURCE_APPLICATION:     printf("Source: Application"); break;
      case GL_DEBUG_SOURCE_OTHER:           printf("Source: Other"); break;
   } printf("\n");

   switch (type)
   {
      case GL_DEBUG_TYPE_ERROR:               printf("Type: Error"); break;
      case GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR: printf("Type: Deprecated Behaviour"); break;
      case GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR:  printf("Type: Undefined Behaviour"); break;
      case GL_DEBUG_TYPE_PORTABILITY:         printf("Type: Portability"); break;
      case GL_DEBUG_TYPE_PERFORMANCE:         printf("Type: Performance"); break;
      case GL_DEBUG_TYPE_MARKER:              printf("Type: Marker"); break;
      case GL_DEBUG_TYPE_PUSH_GROUP:          printf("Type: Push Group"); break;
      case GL_DEBUG_TYPE_POP_GROUP:           printf("Type: Pop Group"); break;
      case GL_DEBUG_TYPE_OTHER:               printf("Type: Other"); break;
   } printf("\n");

   switch (severity)
   {
      case GL_DEBUG_SEVERITY_HIGH:         printf("Severity: high"); break;
      case GL_DEBUG_SEVERITY_MEDIUM:       printf("Severity: medium"); break;
      case GL_DEBUG_SEVERITY_LOW:          printf("Severity: low"); break;
      case GL_DEBUG_SEVERITY_NOTIFICATION: printf("Severity: notification"); break;
   } printf("\n");
   printf("\n");
}

int main(int argc, char ** argv)
{
   /*    got the below arcan template from arcan/src/platform/arcan/video.c */
   static struct arg_arr* shmarguments;
   struct arcan_shmif_cont arcanShmifControl = arcan_shmif_open(
       SEGID_APPLICATION, SHMIF_ACQUIRE_FATALFAIL, &shmarguments);
   if (arcanShmifControl.addr == NULL){
      fprintf(stderr,"shmif open failed : couldn't connect to parent\n");
      return EXIT_FAILURE;
   }
   printf("window dimensions: width %lu, height: %lu\n", (unsigned long)arcanShmifControl.w, (unsigned long)arcanShmifControl.h);

   /* setup requires shmif connection as we might get metadata that way */
   enum shmifext_setup_status status;
   if ((status = arcan_shmifext_headless_setup(&arcanShmifControl,
					       arcan_shmifext_headless_defaults())) != SHMIFEXT_OK){
      printf("headless graphics setup failed, code: %d\n", status);
      fprintf(stderr,
	      "EGL: Failed to create context: %s",
	      getEGLErrorString(eglGetError()));
      arcan_shmif_drop(&arcanShmifControl);
      return EXIT_FAILURE;
   }

   /*    if (!arcan_shmif_resize(&arcanShmifControl, arcanShmifControl.w, arcanShmifControl.h)){ */
   /*       arcan_shmif_drop(&arcanShmifControl); */
   /*    } */

   GLint flags;
   glGetIntegerv(GL_CONTEXT_FLAGS, &flags);
   checkError();
   if (flags & GL_CONTEXT_FLAG_DEBUG_BIT) {
      printf ("OpenGL debug flag set\n");
      // initialize debug output
      glEnable(GL_DEBUG_OUTPUT);
      glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
      checkError();
      glDebugMessageCallback((GLDEBUGPROC)debugOutput, NULL);
      checkError();
   } else {printf ("OpenGL debug flag not set\n");}

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
   checkError();
   printf("GL_VENDOR: %s\n",response);
   response = (char *)glGetString(GL_RENDERER);
   checkError();
   printf("GL_VENDOR: %s\n",response);
   response = (char *)glGetString(GL_VERSION);
   checkError();
   printf("GL_VERSION: %s\n",response);

   /* for sample code : http://www.songho.ca/opengl/gl_fbo.html */
   GLuint colorTextureName;
   printf("start building texture\n");
   glGenTextures(1, &colorTextureName);
   checkError();
   glBindTexture(GL_TEXTURE_2D, colorTextureName);
   checkError();
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
   checkError();
   glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   checkError();
   glTexImage2D(	GL_TEXTURE_2D,
			0,
			GL_RGBA,
			/* width, height */
			arcanShmifControl.w, arcanShmifControl.h,
			0,
			GL_RGBA,
			GL_UNSIGNED_BYTE,
			NULL);
   checkError();
   glBindTexture(GL_TEXTURE_2D, 0);
   checkError();
   printf("finished building texture\n");

   printf("start building renderbuffer\n");
   GLuint depthRenderBufferName;
   glGenRenderbuffers(1, &depthRenderBufferName);
   checkError();
   glBindRenderbuffer(GL_RENDERBUFFER, depthRenderBufferName);
   checkError();
   glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT24, arcanShmifControl.w, arcanShmifControl.h);
   checkError();
   glBindRenderbuffer(GL_RENDERBUFFER, 0);
   checkError();
   printf("finished building renderbuffer\n");

   printf("headless graphics generating framebuffer\n");
   GLuint framebufferName;
   glGenFramebuffers(1, &framebufferName);
   checkError();
   glBindFramebuffer(GL_FRAMEBUFFER, framebufferName);
   checkError();

   /* attach the texture to the above created framebuffer */
   glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, colorTextureName, 0);
   checkError();

   /* attach the renderbuffer to the depth attachment point of the
    * above created framebuffer */
   glFramebufferRenderbuffer(GL_DRAW_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depthRenderBufferName);
   checkError();

   CHECK_FRAMEBUFFER_STATUS();
   /* switch back to the default framebuffer ? is this necessary? */
   /*    glBindFramebuffer(GL_FRAMEBUFFER, 0); */
   /*    checkError(); */
   printf("headless graphics finished generating framebuffer\n");

   printf("headless graphics start rendering to framebuffer\n");
   glBindFramebuffer(GL_FRAMEBUFFER, framebufferName);
   checkError();

   printf("headless graphics start rendering\n");
   /* clear the color buffer and paint it green */
   glClearColor(0.0, 1.0, 0.0, 1.0);
   checkError();
   glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
   checkError();
   glBindFramebuffer(GL_FRAMEBUFFER, 0);
   checkError();
   GLenum ErrorCheckValue = glGetError();
   if (ErrorCheckValue != GL_NO_ERROR)
   {
      fprintf(stderr, "ERROR: Could not draw the triangles: %i \n", ErrorCheckValue);
      exit(-1);
   }

   /* assuming that this call should always work */
   glFlush();
   checkError();
   printf("headless graphics after glFlush\n");

   /*    need to render from the texture */
   int eglsignalStatus = 0;
   uintptr_t display;

   /* https://github.com/letoram/SDL2/blob/master/src/video/arcan/SDL_arcanopengl.c#L85 */
   /* <letoram> problem is that all these interfaces are in flux */
   /* <letoram> that the wayland etc. people are somewhat claiming that these things are 'ready to be default' is complete nonsense */
   /* <letoram> they rely on a bunch of hacks they did in Mesa because they're also mesa devs, and the rest should just 'adopt' */
   /* <letoram> and nvidia didn't want to have any of that, so now we have 2 competing solutions, none of them are complete - and the devs involved are starting to say "ok, we might need to agree on something" */
   /* <letoram> so until whatever the real buffer passing solution should be, we're sortof stuck relying on things like this */
   /* <letoram> signalhandle requires a descriptor, */
   /* <letoram> there's a convenience function that does that job for you */
   /* <letoram> ah sorry saw now that you figured that out (the eglsignal) */
   /* <letoram> vidp is allocated on the resize call */
   /* <letoram> and you need to flush before you signal */
   /* <letoram> otherwise the data is just pending on the gpu somewhere */
   /* <letoram> ah. another thing */
   /* <letoram> eglsignal and signal are the same operation, one is if the data is being sent from the GPU (eglsignal or signalhandle) and the other if that fails (and for software- */
   /*           only drawing using other tools than opengl) */
   /* <letoram> so if you do eglsignal, arcan will grab that handle, map to a texture and draw.. */
   /* <letoram> gl workflow: */
   /* <letoram> init: setup EGL with whatever, build and bind FBO */
   /* <letoram> connect/open arcan, arcan_shmif_resize to negotiate buffers */
   /* <letoram> draw loop: queue your GL draw calls, glFlush(), then shmifext_eglsignal */
   /* <letoram> that should be enough */
   /* <letoram> eglsignal *can* fail but the failure is asynchronous, it fails if the display server determines that it can't use handle-passing anymore */
   /* <letoram> if it's compositing on a different GPU, or the GPU has run out of buffers that can be mapped to a handle and so on - on multi-GPU this is extremely complicated */
   /* <letoram> (and the reason for the GBM vs EGLStreams debate if you follow the politics going around) */
   /* <letoram> if handle passing isn't working, then the fallback resort is the whole glReadPixels (or glGetTexImage2D, ...) into vidp and then signal */
   /* <letoram> but that is a strong performance penalty (but better than nothing at all) */
   if (!arcan_shmifext_egl_meta(&arcanShmifControl, &display, NULL, NULL)) {
      printf("arcan_shmifext_egl_meta returned false. Hence, exiting..\n");
      return 0;
   }
   eglsignalStatus = arcan_shmifext_eglsignal(&arcanShmifControl,
					      display,
					      SHMIF_SIGVID, colorTextureName);
   if (eglsignalStatus == -1){
      printf("arcan_shmifext_eglsignal returned failure %i, Trying the slower way of copying the texture to the video memory (vidp) and then signal the gpu to render it\n",eglsignalStatus);
      /* read the texture image rendered back to the main cpu memory (where vidp points to) */
      glBindTexture(GL_TEXTURE_2D, colorTextureName);
      checkError();
      /* vidp is allocated on the resize call */
      glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA,
		    GL_UNSIGNED_BYTE, (void*) arcanShmifControl.vidp);
      checkError();
      glBindTexture(GL_TEXTURE_2D, 0);
      checkError();

      arcan_shmif_signal(&arcanShmifControl, SHMIF_SIGVID);
   } else if (eglsignalStatus > 0){
      printf("arcan_shmifext_eglsignal took %i milliseconds\n",eglsignalStatus);
   } else {
      printf("arcan_shmifext_eglsignal returned unexpected value: %i\n",eglsignalStatus);
   }

   /*    eglSwapBuffers(display, surface); */
   printf("headless graphics sleeping\n");

   sleep(10);

   printf("headless graphics after sleeping\n");
   /*    is this good enough for freeing the above created resources */
   arcan_shmif_drop(&arcanShmifControl);

   /* detach the texture */
   glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0);
   checkError();
   glFramebufferRenderbuffer(GL_FRAMEBUFFER, 0, GL_RENDERBUFFER, 0);
   checkError();
   glBindRenderbuffer(GL_RENDERBUFFER, 0);
   checkError();
   glDeleteRenderbuffers(1, &depthRenderBufferName);
   checkError();

   glBindFramebuffer(GL_FRAMEBUFFER,0);
   checkError();
   glDeleteFramebuffers(1,&framebufferName);
   checkError();
   return EXIT_SUCCESS;
}

