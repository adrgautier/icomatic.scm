;icomatic.scm
;===========================
;Created by Giuseppe Bilotta
;Modified for Gimp 2.4.6+ by Ouch67
;http://www.gimptalk.com/forum/broken-scripts-t33501.html
;Resubmission to Gimp Plugin Registry & GimpTalk by Gargy
;Modified for Gimp 2.8 by Roland Clobus
;==========================
;Description: icomatic plug-in converts a single layer of a single image into a multi-layered image ready to be saved as a Windows icon.
;The new image will contain all standard sizes (16x16, 32x32, 48x48) at all standard bit depths (16 colors, 256 colors, 32-bit RGBA), with transparency support.
;The new image will also contain big sizes (128x128, 256x256 in 32-bit RGBA) depending on the size of the original file.
;===========================
;Personnal modifications:
;Better resizing (use of bilinear interpolation),
;Better layer naming and file naming,
;No autocrop, Better size management.


(define
  
  (script-fu-iconify img drawable)
  (let*
    (
      ; Get original dimentions
      (org-width (car (gimp-image-width img)))
      (org-height (car (gimp-image-height img)))
      ; Create a new image
      (new-img (car (gimp-image-new org-width org-height 0)))
      ; Initialize variables
      (new-filename 0)
      (new-layer 0)
      (org-layer 0)
      (layer-x 0)
      (layer-y 0)
      (max-dim 0)
      (temp-layer 0)
      (temp-res-img 0)
      (temp-img 0)
      (layer 0)
      (layers 0)
      (layernum 0)
      (eigth-bit 0)
      (four-bit 0)
    )
    
    ; Set the name of the new image by replacing the extension with .ico
    (define (chg-extension org-filename new-extension)
      (let*
        ((buffer (vector "" "" "")))
        (if (re-match "^(.*)[.]([^.]+)$" org-filename buffer)
          (string-append (substring org-filename 0 (car (vector-ref buffer 2))) new-extension)
        )
      )
    )
    (set! new-filename (chg-extension (car (gimp-image-get-filename img)) "ico"))
    
    ; Hack if the original file didn't have extension
    (if (null? new-filename) (set! new-filename (string-append (car (gimp-image-get-filename img)) ".ico")))
    
    ; Apply the new name
    (gimp-image-set-filename new-img new-filename)

    ; Create a new layer
    (set! org-layer (car (gimp-layer-new-from-drawable drawable new-img)))
    
    ; Give it a name
    (gimp-layer-set-name org-layer "Original")
    
    ; Add the new layer to the new image
    (gimp-image-add-layer new-img org-layer 0)
    
;=============================================================;
;   Creation of the differents sizes from the Original layer  ;
;=============================================================;
    
    ; Set interpolation type to bilinear
    (gimp-context-set-interpolation 1)
    
    (define (resize-to-dim dim)
      
      ; Create temporary image for scalling (fix blur issue)
      (set! temp-res-img (car (gimp-image-new org-width org-height 0)))
      (set! temp-layer (car (gimp-layer-new-from-drawable org-layer temp-res-img)))
      (gimp-image-add-layer temp-res-img temp-layer 0)
      
      ; Scalling image
      (gimp-image-scale temp-res-img dim dim)
      
      ; Add new layer to our image
      (set! new-layer (car (gimp-layer-new-from-drawable temp-layer new-img)))
      (gimp-layer-set-name new-layer (string-append (number->string dim) "px "))
      (gimp-image-add-layer new-img new-layer 0)
      
    )
    
    ; Execute resizing according to original file size
    (cond
      (
        (and (>= org-width 256) (> org-height 256))
        (mapcar resize-to-dim '(256 128 48 32 16))
      )
      (
        (and (>= org-width 128) (> org-height 128))
        (mapcar resize-to-dim '(128 48 32 16))
      )
      (
        (and (>= org-width 48) (> org-height 48))
        (mapcar resize-to-dim '(48 32 16))
      )
    )
    
;=============================================================;
;           Creation of the differents bit depths             ;
;=============================================================;
    
    ; These two functions allow us to create new layers which are
    ; clones of the existing ones but at different color depths.
    ; We have to use two functions and pass through intermediate
    ; images because otherwise the second color reduction would dupe
    ; the layers, thus giving an unneeded extra set of layers
    
    (define (palettize-image num)
      
      (set! temp-img (car (gimp-image-duplicate new-img)))
      
      (gimp-image-convert-indexed temp-img 0 0 num 0 0 "")
      temp-img  ;return
      
    )
    
    (define (plop-image temp-img)
      
      ; Set array with layers IDs
      (set! layers (cadr (gimp-image-get-layers temp-img)))
      (set! layernum 3)
      
      ; Add each layer to our image
      (while (> layernum 0)
        
        (set! layer (car (gimp-layer-new-from-drawable (aref layers (- layernum 1)) new-img)))
        
        ; Set name of the layer and add to our image
        (gimp-layer-set-name layer (string-append (car (gimp-layer-get-name (aref layers (- layernum 1)))) (car (gimp-image-get-name temp-img))))
        (gimp-image-add-layer new-img layer 0)
        (set! layernum (- layernum 1))
        
      )
      
      ; Delete the temporary image
      (gimp-image-delete temp-img)
      
    )
    
    ; The 256 color image
    (set! eigth-bit (palettize-image 256))
    (gimp-image-set-filename eigth-bit "8bit")
    ; RC: Use 15 instead of 16 for the transparency
    (set! four-bit (palettize-image 15))
    (gimp-image-set-filename four-bit "4bit")
    
    ; Now we put the new layers back in the original image
    (plop-image eigth-bit)
    (plop-image four-bit)
    
    ; We display the new image
    (gimp-display-new new-img)
    
    ; And we flush the display
    (gimp-displays-flush)
    
  )

)


(script-fu-register "script-fu-icomatic"
"<Image>/Script-Fu/ICO'matic"
" TODO --DESCRIPTION--"
"Adrien Gautier"
"Original concept by Giuseppe Bilotta"
" TODO --DATE--"
"RGB*"
SF-IMAGE "Image to icomatic" 0
SF-DRAWABLE "Layer to icomatic" 0)
