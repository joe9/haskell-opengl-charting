
#define WANT_ARCAN_SHMIF_HELPER

#include <stdlib.h>
#include <unistd.h>
#include <EGL/egl.h>
#include <GL/gl.h>
#include "shmif/arcan_shmif.h"
#include "shmif/shmif_privext.h"

int main(int argc, char ** argv)
{
   /*    EGLDisplay display; */
   /*    EGLConfig config; */
   /*    EGLContext context; */
   /*    EGLSurface surface; */
   /*    NativeWindowType native_window; */
   /*    EGLint num_config; */

   /*    /\* get an EGL display connection *\/ */
   /*    display = eglGetDisplay(EGL_DEFAULT_DISPLAY); */
   /*    /\* initialize the EGL display connection *\/ */
   /*    eglInitialize(display, NULL, NULL); */
   /*    /\* get an appropriate EGL frame buffer configuration *\/ */
   /*    eglChooseConfig(display, attribute_list, &config, 1, &num_config); */
   /*    /\* create an EGL rendering context *\/ */
   /*    context = eglCreateContext(display, config, EGL_NO_CONTEXT, NULL); */
   /*    /\* create a native window *\/ */
   /*    native_window = createNativeWindow(); */
   /*    /\* create an EGL window surface *\/ */
   /*    surface = eglCreateWindowSurface(display, config, native_window, NULL); */
   /*    /\* connect the context to the surface *\/ */
   /*    eglMakeCurrent(display, surface, surface, context); */

   /*    all the above should be done by the below. ensure that all the calls are taken care of by stepping through with gdb */
   /* get an EGL display connection */
   /*       display = eglGetDisplay(EGL_DEFAULT_DISPLAY); */
   /* initialize the EGL display connection */
   /*    eglInitialize(display, NULL, NULL); */
   /* get an appropriate EGL frame buffer configuration */
   /*    eglChooseConfig(display, attribute_list, &config, 1, &num_config); */
   /* create an EGL rendering context */
   /*    context = eglCreateContext(display, config, EGL_NO_CONTEXT, NULL); */
   /* create a native window */
   /*    native_window = createNativeWindow(); */
   /* create an EGL window surface */
   /*    surface = eglCreateWindowSurface(display, config, native_window, NULL); */
   /* connect the context to the surface */
   /*    eglMakeCurrent(display, surface, surface, context); */

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

   /* clear the color buffer */
   /*    glClearColor(1.0, 1.0, 0.0, 1.0); */
   /*    glClear(GL_COLOR_BUFFER_BIT); */

   /* assuming that this call should always work */
   glFlush();
   /*    need to render to a texture */
   /*    eglSwapBuffers(display, surface); */
   sleep(10);

   /*    is this good enough for freeing the above created resources */
   arcan_shmif_drop(&arcanShmifControl);
   return EXIT_SUCCESS;
}

