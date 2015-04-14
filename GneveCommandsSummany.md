# Video commands: #
| V | Visit video file and start playing |
|:--|:-----------------------------------|
| L | Pause/Play |
| J | 1 frame back and pause |
| K | 1 frame forward and pause |
| Q | 1 second back and pause |
| W | 1 second forward and pause |
| A | 5 seconds back and pause |
| S | 5 seconds forward and pause |
| C | Goto timecode of current point |
| G | Goto timecode of user input |

> ## Layout summary: ##
```
 Q W 
  A S     G   J K L
       C V   
```

# Editing commands: #

| E | Mark start of a section |
|:--|:------------------------|
| R | Mark end of a section |
| H | Write marked section to EDL buffer |
| Z | Goto start of marked section and pause |
| X | Goto end of marked section and pause |

> ## Layout summary: ##
```
     E R 
             H
   Z X
```

# Render commands: #
| U | Render active region |
|:--|:---------------------|
| I | Render whole buffer |
| O | Save rendered video |
| P | Play rendered video |

> ## Layout summary: ##
```
             U I O P
```