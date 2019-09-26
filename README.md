# Ivy Personal Dictionary

Creating personal dictionary with your favorite words.  
You can easily add/remove words and paste them via ivy.

## Setup

`git clone` and edit your init.el as below.

```elisp
(add-to-list 'load-path "YOUR PATH")
(require 'ivy-personal-dictionary)

;; optional
(global-set-key [YOUR KEY] 'personal-dic-save)
(global-set-key [YOUR KEY] 'personal-dic-load)
```

## Usage

### M-x personal-dic-save

+ thing-at-point: When your cursor on the symbol, you can add it
+ region: When you selected the region, you can add the region
+ other: You can input words in minibuffer

### M-x personal-dic-load

You can load saved-words and select/paste them via ivy.
