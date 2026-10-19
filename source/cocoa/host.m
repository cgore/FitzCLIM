// Copyright (c) 2026, Christopher Mark Gore,
// Soli Deo Gloria,
// All rights reserved.
//
// 22 Forest Glade Court, Saint Charles, Missouri 63304 USA.
// Web: http://cgore.com
// Email: cgore@cgore.com
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//
//     * Neither the name of Christopher Mark Gore nor the names of other
//       contributors may be used to endorse or promote products derived from
//       this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#import <AppKit/AppKit.h>
#import <objc/runtime.h>

/* NSRect and NSPoint are passed in registers that SBCL does not fill
   for a structure pointer.  The functions here build those values. */

void fitz_view_frame(void *view, double *x, double *y, double *w, double *h) {
  NSRect rect = [(id)view frame];
  *x = rect.origin.x;
  *y = rect.origin.y;
  *w = rect.size.width;
  *h = rect.size.height;
}

void fitz_set_frame(void *view, double x, double y, double w, double h) {
  [(id)view setFrame:NSMakeRect(x, y, w, h)];
}

void fitz_set_content_size(void *window, double w, double h) {
  [(id)window setContentSize:NSMakeSize(w, h)];
}

void *fitz_make_window(double x, double y, double w, double h, const char *title) {
  NSRect rect = NSMakeRect(x, y, w, h);
  NSWindow *window = [[NSWindow alloc] initWithContentRect:rect
                                                  styleMask:15
                                                    backing:NSBackingStoreBuffered
                                                      defer:NO];
  [window setTitle:[NSString stringWithUTF8String:title ? title : ""]];
  return (void *)window;
}

void *fitz_make_view(const char *class_name, double x, double y, double w, double h) {
  Class class = objc_getClass(class_name);
  return (void *)[[class alloc] initWithFrame:NSMakeRect(x, y, w, h)];
}

void fitz_event_point(void *event, void *view, double *x, double *y) {
  NSPoint point = [(id)view convertPoint:[(id)event locationInWindow] fromView:nil];
  *x = point.x;
  *y = point.y;
}

int fitz_event_type(void *event) {
  return (int)[(NSEvent *)event type];
}

unsigned long fitz_event_modifiers(void *event) {
  return (unsigned long)[(NSEvent *)event modifierFlags];
}

unsigned short fitz_event_key_code(void *event) {
  return [(NSEvent *)event keyCode];
}

unsigned short fitz_event_char(void *event) {
  NSString *characters = [(NSEvent *)event characters];
  if ([characters length] == 0)
    return 0;
  return [characters characterAtIndex:0];
}

static NSEvent *mouse_event(id view, NSEventType type, double x, double y,
                            unsigned long modifiers) {
  NSPoint window_point = [view convertPoint:NSMakePoint(x, y) toView:nil];
  return [NSEvent mouseEventWithType:type
                             location:window_point
                        modifierFlags:(NSEventModifierFlags)modifiers
                            timestamp:[[NSProcessInfo processInfo] systemUptime]
                         windowNumber:[[view window] windowNumber]
                              context:nil
                          eventNumber:1
                           clickCount:1
                             pressure:1.0f];
}

void fitz_send_mouse(void *view, int type, double x, double y, unsigned long modifiers) {
  id target = (id)view;
  NSEvent *event = mouse_event(target, (NSEventType)type, x, y, modifiers);
  switch ((NSEventType)type) {
  case NSEventTypeLeftMouseDown: [target mouseDown:event]; break;
  case NSEventTypeLeftMouseUp: [target mouseUp:event]; break;
  case NSEventTypeRightMouseDown: [target rightMouseDown:event]; break;
  case NSEventTypeRightMouseUp: [target rightMouseUp:event]; break;
  case NSEventTypeOtherMouseDown: [target otherMouseDown:event]; break;
  case NSEventTypeOtherMouseUp: [target otherMouseUp:event]; break;
  case NSEventTypeMouseMoved: [target mouseMoved:event]; break;
  default: break;
  }
}

void fitz_send_key(void *view, int type, unsigned short code, unsigned short character,
                   unsigned long modifiers) {
  id target = (id)view;
  unichar unit = character;
  NSString *text = character ? [NSString stringWithCharacters:&unit length:1] : @"";
  NSEvent *event = [NSEvent keyEventWithType:(NSEventType)type
                                     location:NSMakePoint(0, 0)
                                modifierFlags:(NSEventModifierFlags)modifiers
                                    timestamp:[[NSProcessInfo processInfo] systemUptime]
                                 windowNumber:[[target window] windowNumber]
                                      context:nil
                                   characters:text
                  charactersIgnoringModifiers:text
                                    isARepeat:NO
                                      keyCode:code];
  if ((NSEventType)type == NSEventTypeKeyDown)
    [target keyDown:event];
  else
    [target keyUp:event];
}

void fitz_post_wakeup(void *window) {
  NSEvent *event = [NSEvent otherEventWithType:NSEventTypeApplicationDefined
                                      location:NSMakePoint(0, 0)
                                 modifierFlags:0
                                     timestamp:[[NSProcessInfo processInfo] systemUptime]
                                  windowNumber:[(id)window windowNumber]
                                       context:nil
                                       subtype:0
                                         data1:0
                                         data2:0];
  [NSApp postEvent:event atStart:YES];
}

int fitz_poll_send(int *type) {
  NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny
                                      untilDate:[NSDate distantPast]
                                         inMode:NSDefaultRunLoopMode
                                        dequeue:YES];
  if (!event)
    return 0;
  *type = (int)[event type];
  [NSApp sendEvent:event];
  return 1;
}
