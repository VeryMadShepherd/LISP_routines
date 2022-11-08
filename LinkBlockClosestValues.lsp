(vl-load-com)

;This subroutine used to convert separate elements (block + related text) to single one (block with attribute). For now it is fit to pipeline schemes, instrumentation in particular. 
;Thats why it finds two elements nearby
;
;textLayer: layer that consists source texts that should be converted to attributes
;blockLayer: layer that consists blocks with 'attrName' attributeText
;drawing must consist "DELETE" layer -> used texts will be pur into this layer 
(defun LBCV 
	(
		textLayer 
		blockLayer
		attrName
		/ 
		ss1 
		blocks ;blocks to which attributes will be assigned
		texts ;attributes values to found
		i 
		block 
		blockPoint
		attributesTexts
		attributeText
	);end of declaration
	
	;source of this subroutine: lee-mac.com
	(defun LM:vl-setattributevalue ( blk tag val )
		(setq tag (strcase tag))
		(vl-some
			'(lambda ( att )
				(if (= tag (strcase (vla-get-tagstring att)))
					(progn (vla-put-textstring att val) val)
				)
			)
			(vlax-invoke blk 'getattributes)
		)
	);end defun
	
	;values: ssget set of elements
	;zeroPoint: block coordinates
	;returns two elements closest to zeroPoint
	(defun FindClosestTexts (values zeroPoint / j minimalValue secondValue minimalDistance secondDistance textPoint result)
		(setq minimalDistance 0.0)
		(setq secondDistance 0.0)
		(setq j 0)
		(while (setq text (ssname values j))
			(setq textPoint (cdr (assoc 10 (entget text))))
			(setq currentDistance (distance textPoint zeroPoint))
			(cond 
				((= minimalDistance 0.0)
					(setq minimalDistance currentDistance)
					(setq minimalValue text)		
				);end
				((< currentDistance minimalDistance)
					(setq minimalDistance currentDistance)
					(setq minimalValue text)		
				);end
			); end cond	
			(setq j (1+ j))
		);end while
		(setq j 0)
		;I was too lazy to put it in a separate function and make code shorter
		(while (setq text (ssname values j))
			(setq textPoint (cdr (assoc 10 (entget text))))
			(setq currentDistance (distance textPoint zeroPoint))
			(cond 
				((and(= secondDistance 0.0)(> currentDistance minimalDistance))
					(setq secondDistance currentDistance)
					(setq secondValue text)		
				);end
				((and(< currentDistance secondDistance)(> currentDistance minimalDistance))
					(setq secondDistance currentDistance)
					(setq secondValue text)		
				);end
			); end cond	
			(setq j (1+ j))
		);end while
		(setq result (list minimalValue secondValue))
	);end defun
	(setq blocks (ssget "_X" (list (cons 0 "INSERT")(cons 8 blockLayer))))
	(setq texts (ssget "_X" (list (cons 0 "TEXT")(cons 8 textLayer))))
	(setq i 0)
	(while (setq block (ssname blocks i))
			(setq blockPoint (cdr (assoc 10 (entget block))))
			(setq attributesTexts (FindClosestTexts texts blockPoint))
			(setq attributeText 
				(strcat 
					(vla-get-textstring (vlax-ename->vla-object (car attributesTexts))) 
					"\n"
					(vla-get-textstring (vlax-ename->vla-object (cadr attributesTexts)))
				);end strcat
			);end setq
			(LM:vl-setattributevalue (vlax-ename->vla-object block) attrName attributeText)
			(vlax-put-property (vlax-ename->vla-object (car attributesTexts)) 'layer "DELETE")
			(vlax-put-property (vlax-ename->vla-object (cadr attributesTexts)) 'layer "DELETE")
			(setq i (1+ i))
	)
)

;example
(defun c:kipreplace (/ )
	(LBCV "KIP" "KIP_GREEN" "KIP_NAME")
	(LBCV "KIP" "KIP_RED" "KIP_NAME")
	(LBCV "KIP" "KIP_YELLOW" "KIP_NAME")
	(print "DONE")
)