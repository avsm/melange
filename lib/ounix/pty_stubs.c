/*
 * Copyright (c) 2004 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdio.h>
#include <errno.h>
#include <paths.h>
#include <fcntl.h>
#include <string.h>
#include <termios.h>
#include <sys/ioctl.h>

#ifdef HAVE_PTY_H
#  include <pty.h>
#endif

#ifdef HAVE_UTIL_H
#  include <util.h>
#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/signals.h>

/*
 * type pty = {
 *   masterfd: int;
 *   slavefd: int;
 *   name: string;
 * }
 */

/* Raise a Pty_error exception; be careful that errno is something
 * meaningful when this function is called */
static void
pty_error(char *msg)
{
    char errbuf[1024];
    snprintf(errbuf, sizeof errbuf, "%s: %s", msg, strerror(errno));
    raise_with_string(*caml_named_value("pty_error"), errbuf);
}

/* Wrapper for openpty(3), returns variant type pty */
value
pty_open_pty(value unit)
{
    CAMLparam1 (unit);
    char namebuf[64];
    int i, masterfd, slavefd;
    CAMLlocal1(ret);

    i = openpty(&masterfd, &slavefd, namebuf, NULL, NULL);
    if (i < 0)
        pty_error("openpty");

    ret = alloc_small(3, 0);
    Store_field(ret, 0, Val_int(masterfd));
    Store_field(ret, 1, Val_int(slavefd));
    Store_field(ret, 2, copy_string(namebuf));

    CAMLreturn (ret);
}

/* Takes variant type pty and switches controlling terminals.
 * Raises Pty_error on error, returns unit otherwise */
value
pty_switch_controlling_tty(value pty)
{
    CAMLparam1 (pty);
    int fd, ttyfd;
    char *ttyname;
    
    /* Disconnect from the old tty */
    fd = open(_PATH_TTY, O_RDWR | O_NOCTTY);
    if (fd != -1) {
        ioctl(fd, TIOCNOTTY, NULL);
        close(fd);
    }
    
    if (setsid() == -1)
        pty_error("setsid");
    
    /* Verify that we have successfully disconnected */
    fd = open(_PATH_TTY, O_RDWR | O_NOCTTY);
    if (fd != -1)
        pty_error("Failed to disconnect original tty");
    
    ttyfd = Int_val(Field(pty, 1));
    ttyname = String_val(Field(pty, 2));

    /* Switch to the new tty */    
    if (ioctl(ttyfd, TIOCSCTTY, NULL) < 0)
        pty_error("TIOCSCTTY");
    
    fd = open(ttyname, O_RDWR);
    if (fd == -1)
        pty_error(ttyname);
    else
        close(fd);
    
    /* Verify that the tty is now the controller */
    fd = open(_PATH_TTY, O_WRONLY);
    if (fd == -1)
        pty_error("Failed to set controlling tty");
    else
        close(fd);
        
    CAMLreturn (Val_unit);
}

/* Change the window size of the pty, returns unit */
value
pty_window_size(value pty, value pty_window)
{
    CAMLparam2 (pty, pty_window);
    int ptyfd;
    struct winsize w;
    
    w.ws_row = Int_val(Field(pty_window, 0));
    w.ws_col = Int_val(Field(pty_window, 1));
    w.ws_xpixel = Int_val(Field(pty_window, 2));
    w.ws_ypixel = Int_val(Field(pty_window, 3));
    
    ptyfd = Int_val(Field(pty, 0));
    ioctl(ptyfd, TIOCSWINSZ, &w);
    
    CAMLreturn (Val_unit);
}

