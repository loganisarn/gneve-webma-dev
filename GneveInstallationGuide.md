This page is outdated. Please check GNEVE CVS at Savannah for updated version.

/
/
/
/
/
/
/
/
/
/
/
/
/
/
/
/
/
/
/
/
/
/



# Introduction #

GNU Emacs Video Editor mode (GNEVE) is an elisp application based on a patched mplayer and avidemux. Mplayer previews source videos that can be controlled using keyboard commands. Avidemux renders Edit Decision List using GNU Emacs video editor mode.

A patched version of mplayer and avidemux 2.4 has to be properly installed before using gneve.el.


# Installing gneve.el #

## stable gneve.el version ##

```
wget http://1010.co.uk/gneve.el
```

## webma devel version ##

```
svn checkout http://gneve-webma-dev.googlecode.com/svn/trunk/ gneve-webma-dev
```

Put something similar to the following in your ~/.emacs to use this file:

```
(add-to-list 'load-path "~/path/to/gneve-mode/")
(require 'gneve-mode)
```



# Dependency #

gnevel.el recommends mplayer 1.0 and avidemux 2.4 using lastest svn version

# Patching and installing mplayer #

First you have to patch mplayer in order to enable timecode precision up to 6 decimal digits. http://www.mplayerhq.hu/DOCS/HTML-single/en/MPlayer.html

## checkout mplayer ##

```
  svn checkout svn://svn.mplayerhq.hu/mplayer/trunk mplayer
```

> ## patch mplayer ##

```
--- command.c	(revision 23382)
+++ command.c	(working copy)
@@ -2325,7 +2325,7 @@
 		    pos =
 			playing_audio_pts(sh_audio, mpctx->d_audio,
 					  mpctx->audio_out);
-		mp_msg(MSGT_GLOBAL, MSGL_INFO, "ANS_TIME_POSITION=%.1f\n", pos);
+		mp_msg(MSGT_GLOBAL, MSGL_INFO, "ANS_TIME_POSITION=%.6f\n", pos);
 	    }
 	    break;
```

## install mplayer into /usr/local/bin ##

```
./configure
make
su
make install
```

# Installing avidemux #

Avidemux may require some libriaries that has to be available. Please read instructions at http://www.avidemux.org/admWiki/index.php?title=Compiling_Avidemux.

## checkout avidemux ##

```
  svn checkout svn://svn.berlios.de/avidemux/branches/avidemux_2.4_branch/
```

## install avidemux into /usr/local/bin ##
make -f Makefile.dist
./configure
make
su
make install```