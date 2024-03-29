;;; zencoding-mode.el --- Unfold CSS-selector-like expressions to markup

;; Copyright (C) 2009, Chris Done

;; Version: 0.5.1
;; Author: Chris Done <chrisdone@gmail.com>
;; URL: https://github.com/rooney/zencoding
;; Last-Updated: 2011-12-31 Sat
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Unfold CSS-selector-like expressions to markup. Intended to be used
;; with sgml-like languages; xml, html, xhtml, xsl, etc.
;;
;; See `zencoding-mode' for more information.
;;
;; Copy zencoding-mode.el to your load-path and add to your .emacs:
;;
;;    (require 'zencoding-mode)
;;
;; Example setup:
;;
;;    (add-to-list 'load-path "~/Emacs/zencoding/")
;;    (require 'zencoding-mode)
;;    (add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
;;    (add-hook 'html-mode-hook 'zencoding-mode)
;;    (add-hook 'css-mode-hook  'zencoding-mode)
;;
;; Enable the minor mode with M-x zencoding-mode.
;;
;; See ``Test cases'' section for a complete set of expression types.
;;
;; If you are hacking on this project, eval (zencoding-test-cases) to
;; ensure that your changes have not broken anything. Feel free to add
;; new test cases if you add new features.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; Modified by Lennart Borgman.
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst zencoding-mode:version "0.5.1")

;; Include the trie data structure for caching
;(require 'zencoding-trie)

(require 'cl)

(defmacro zencoding-defparameter (symbol &optional initvalue docstring)
  `(progn
     (defvar ,symbol nil ,docstring)
     (setq   ,symbol ,initvalue)))

(defun zencoding-join-string (lis joiner)
  (mapconcat 'identity lis joiner))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic parsing macros and utilities

(defmacro zencoding-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@(or else-forms '(it)))))

(defmacro zencoding-pif (test-form then-form &rest else-forms)
  "Parser anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if (not (eq 'error (car it))) ,then-form ,@(or else-forms '(it)))))

(defmacro zencoding-parse (regex nums label &rest body)
  "Parse according to a regex and update the `input' variable."
  `(zencoding-aif (zencoding-regex ,regex input ',(number-sequence 0 nums))
                  (let ((input (elt it ,nums)))
                    ,@body)
                  `,`(error ,(concat "expected " ,label))))

(defmacro zencoding-run (parser then-form &rest else-forms)
  "Run a parser and update the input properly, extract the parsed
   expression."
  `(zencoding-pif (,parser input)
                  (let ((input (cdr it))
                        (expr (car it)))
                    ,then-form)
                  ,@(or else-forms '(it))))

(defmacro zencoding-por (parser1 parser2 then-form &rest else-forms)
  "OR two parsers. Try one parser, if it fails try the next."
  `(zencoding-pif (,parser1 input)
                  (let ((input (cdr it))
                        (expr (car it)))
                    ,then-form)
                  (zencoding-pif (,parser2 input)
                                 (let ((input (cdr it))
                                       (expr (car it)))
                                   ,then-form)
                                 ,@else-forms)))

(defun zencoding-regex (regexp string refs)
  "Return a list of (`ref') matches for a `regex' on a `string' or nil."
  (if (string-match (concat "^" regexp "\\([^\n]*\\)$") string)
      (mapcar (lambda (ref) (match-string ref string))
              (if (sequencep refs) refs (list refs)))
    nil))
;; src/snippets.el
;; This file is generated from conf/snippets.json
;; Don't edit.
(zencoding-defparameter zencoding-snippets
(let ((tbl (make-hash-table :test 'equal)))
(puthash "html" (let ((tbl (make-hash-table :test 'equal)))
(puthash "snippets" (let ((tbl (make-hash-table :test 'equal)))
(puthash "!!!" "<!doctype html>" tbl)
(puthash "!!!xxs" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" tbl)
(puthash "cc:ie6" "<!--[if lte IE 6]>\n\t${child}\n<![endif]-->" tbl)
(puthash "cc:ie" "<!--[if IE]>\n\t${child}\n<![endif]-->" tbl)
(puthash "!!!xs" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" tbl)
(puthash "!!!4t" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">" tbl)
(puthash "cc:noie" "<!--[if !IE]><!-->\n\t${child}\n<!--<![endif]-->" tbl)
(puthash "!!!4s" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" tbl)
(puthash "!!!xt" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" tbl)
tbl) tbl)
(puthash "aliases" (let ((tbl (make-hash-table :test 'equal)))
(puthash "a:link" "a href=http://" tbl)
(puthash "doc4" "html>(head>meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\"+title{Document})" tbl)
(puthash "input:datetime-local" "input type=datetime-local" tbl)
(puthash "input:reset" "input type=reset" tbl)
(puthash "meta:vp" "meta name=viewport content=\"width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0\"" tbl)
(puthash "colg" "colgroup" tbl)
(puthash "btn:s" "button type=submit" tbl)
(puthash "btn:r" "button type=reset" tbl)
(puthash "tr+" "tr>td" tbl)
(puthash "adr" "address" tbl)
(puthash "bdo:l" "bdo dir=ltr" tbl)
(puthash "colgroup+" "colgroup>col" tbl)
(puthash "btn:b" "button type=button" tbl)
(puthash "form:post" "form action method=post" tbl)
(puthash "bdo:r" "bdo dir=rtl" tbl)
(puthash "fig" "figure" tbl)
(puthash "input:radio" "input type=radio" tbl)
(puthash "link:print" "link rel=stylesheet href=print.css media=print" tbl)
(puthash "opt" "option" tbl)
(puthash "input:i" "input:image" tbl)
(puthash "figc" "figcaption" tbl)
(puthash "input:f" "input:file" tbl)
(puthash "input:c" "input:checkbox" tbl)
(puthash "input:b" "input:button" tbl)
(puthash "input:t" "input" tbl)
(puthash "input:p" "input:password" tbl)
(puthash "input:s" "input:submit" tbl)
(puthash "input:r" "input:radio" tbl)
(puthash "ifr" "iframe" tbl)
(puthash "emb" "embed" tbl)
(puthash "optg+" "optgroup>option" tbl)
(puthash "cmd" "command" tbl)
(puthash "html:5" "!!!+doc lang=en" tbl)
(puthash "link:atom" "link rel=alternate type=\"application/atom+xml\" title=Atom href=atom.xml" tbl)
(puthash "table+" "table>tr>td" tbl)
(puthash "art" "article" tbl)
(puthash "input:search" "input type=search" tbl)
(puthash "area:r" "area shape=rect coords href alt" tbl)
(puthash "area:p" "area shape=poly coords href alt" tbl)
(puthash "input:date" "input type=date" tbl)
(puthash "input:button" "input type=button" tbl)
(puthash "area:d" "area shape=default href alt" tbl)
(puthash "area:c" "area shape=circle coords href alt" tbl)
(puthash "out" "output" tbl)
(puthash "ftr" "footer" tbl)
(puthash "dlg" "dialog" tbl)
(puthash "script:src" "script src" tbl)
(puthash "form:get" "form action method=get" tbl)
(puthash "meta:utf" "meta http-equiv=Content-Type content=\"text/html;charset=UTF-8\"" tbl)
(puthash "input:time" "input type=time" tbl)
(puthash "menu:toolbar" "menu type=toolbar" tbl)
(puthash "prog" "progress" tbl)
(puthash "input:email" "input type=email" tbl)
(puthash "input:submit" "input type=submit" tbl)
(puthash "leg" "legend" tbl)
(puthash "ol+" "ol>li" tbl)
(puthash "optgroup+" "optgroup>option" tbl)
(puthash "bq" "blockquote" tbl)
(puthash "src" "source" tbl)
(puthash "obj" "object" tbl)
(puthash "dl+" "dl>dt+dd" tbl)
(puthash "acr" "acronym" tbl)
(puthash "input:password" "input type=password" tbl)
(puthash "html:4t" "!!!4t+doc4 lang=en" tbl)
(puthash "input:file" "input type=file" tbl)
(puthash "html:4s" "!!!4s+doc4 lang=en" tbl)
(puthash "tarea" "textarea" tbl)
(puthash "link:favicon" "link icon rel=shortcut type=image/x-icon href=favicon.ico" tbl)
(puthash "input:number" "input type=number" tbl)
(puthash "input:range" "input type=range" tbl)
(puthash "input:image" "input type=image src alt" tbl)
(puthash "ul+" "ul>li" tbl)
(puthash "html:xxs" "!!!xxs+doc4 xmlns=http://www.w3.org/1999/xhtml xml:lang=en" tbl)
(puthash "input:month" "input type=month" tbl)
(puthash "fset" "fieldset" tbl)
(puthash "meta:win" "meta http-equiv=Content-Type content=\"text/html;charset=windows-1251\"" tbl)
(puthash "menu:t" "menu:toolbar" tbl)
(puthash "menu:c" "menu:context" tbl)
(puthash "link:rss" "link rel=alternate type=application/rss+xml title=RSS href=rss.xml" tbl)
(puthash "select+" "select>option" tbl)
(puthash "hdr" "header" tbl)
(puthash "cap" "caption" tbl)
(puthash "det" "details" tbl)
(puthash "link:touch" "link rel=apple-touch-icon href=favicon.png" tbl)
(puthash "str" "strong" tbl)
(puthash "link:css" "link rel=stylesheet href=style.css" tbl)
(puthash "input:week" "input type=week" tbl)
(puthash "optg" "optgroup" tbl)
(puthash "input:datetime" "input type=datetime" tbl)
(puthash "input:h" "input:hidden" tbl)
(puthash "!" "html:5" tbl)
(puthash "html:xml" "html xmlns=http://www.w3.org/1999/xhtml" tbl)
(puthash "btn" "button" tbl)
(puthash "input:url" "input type=url" tbl)
(puthash "menu:context" "menu type=context" tbl)
(puthash "colg+" "colgroup>col" tbl)
(puthash "input:color" "input type=color" tbl)
(puthash "meta:compat" "meta http-equiv=X-UA-Compatible content=\"IE=edge,chrome=1\"" tbl)
(puthash "input:hidden" "input type=hidden" tbl)
(puthash "a:mail" "a href=mailto:" tbl)
(puthash "html:xs" "!!!xs+doc4 xmlns=http://www.w3.org/1999/xhtml xml:lang=en" tbl)
(puthash "html:xt" "!!!xt+doc4 xmlns=http://www.w3.org/1999/xhtml xml:lang=en" tbl)
(puthash "datal" "datalist" tbl)
(puthash "map+" "map>area" tbl)
(puthash "kg" "keygen" tbl)
(puthash "doc" "html>(head>meta charset=UTF-8+title{Document})+body" tbl)
(puthash "datag" "datagrid" tbl)
(puthash "input:text" "input" tbl)
(puthash "input:checkbox" "input type=checkbox" tbl)
(puthash "fst" "fieldset" tbl)
(puthash "sect" "section" tbl)
tbl) tbl)
tbl) tbl)
(puthash "css" (let ((tbl (make-hash-table :test 'equal)))
(puthash "snippets" (let ((tbl (make-hash-table :test 'equal)))
(puthash "bdls" "border-left-style:|;" tbl)
(puthash "bdlw" "border-left-width:|;" tbl)
(puthash "bdli" "border-left-image:url(|);" tbl)
(puthash "bdlc" "border-left-color:#${1:000};" tbl)
(puthash "whsc" "white-space-collapse:|;" tbl)
(puthash "bdtlrs" "border-top-left-radius:|;" tbl)
(puthash "bdblrs" "border-bottom-left-radius:|;" tbl)
(puthash "d:tbc" "display:table-cell;" tbl)
(puthash "wow:n" "word-wrap:none;" tbl)
(puthash "tw" "text-wrap:|;" tbl)
(puthash "tt" "text-transform:${1:uppercase};" tbl)
(puthash "tr" "text-replace:|;" tbl)
(puthash "to" "text-outline:|;" tbl)
(puthash "wow:u" "word-wrap:unrestricted;" tbl)
(puthash "tj" "text-justify:|;" tbl)
(puthash "wow:s" "word-wrap:suppress;" tbl)
(puthash "th" "text-height:|;" tbl)
(puthash "d:tbr" "display:table-row;" tbl)
(puthash "td" "text-decoration:${1:none};" tbl)
(puthash "te" "text-emphasis:|;" tbl)
(puthash "bgc:t" "background-color:transparent;" tbl)
(puthash "list:lr" "list-style-type:lower-roman;" tbl)
(puthash "bdl+" "border-left:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "bxz" "box-sizing:${1:border-box};" tbl)
(puthash "bdbk:c" "border-break:close;" tbl)
(puthash "bgbk:bb" "background-break:bounding-box;" tbl)
(puthash "f" "font:|;" tbl)
(puthash "list" "list-style-type:|;" tbl)
(puthash "trf" "transform:|;" tbl)
(puthash "p" "padding:|;" tbl)
(puthash "bdbi:n" "border-bottom-image:none;" tbl)
(puthash "bdf:r" "border-fit:repeat;" tbl)
(puthash "trs" "transition:${1:prop} ${2:time};" tbl)
(puthash "bdrst" "border-right-style:|;" tbl)
(puthash "zm" "zoom:1;" tbl)
(puthash "to:n" "text-outline:none;" tbl)
(puthash "trf:t" "transform: translate(${1:x}, ${2:y});" tbl)
(puthash "animfm:bt" "animation-fill-mode:both;" tbl)
(puthash "animfm:bh" "animation-fill-mode:both;" tbl)
(puthash "bgcp:cb" "background-clip:content-box;" tbl)
(puthash "lisp:o" "list-style-position:outside;" tbl)
(puthash "lisp:i" "list-style-position:inside;" tbl)
(puthash "d:tbclg" "display:table-column-group;" tbl)
(puthash "bdf" "border-fit:${1:repeat};" tbl)
(puthash "@f" "@font-face {\n\tfont-family:|;\n\tsrc:url(|);\n}" tbl)
(puthash "bdc" "border-color:#${1:000};" tbl)
(puthash "d:rbt" "display:ruby-text;" tbl)
(puthash "bdl" "border-left:|;" tbl)
(puthash "@i" "@import url(|);" tbl)
(puthash "bdi" "border-image:url(|);" tbl)
(puthash "bgsz" "background-size:|;" tbl)
(puthash "@m" "@media ${1:screen} {\n\t|\n}" tbl)
(puthash "fef:eb" "font-effect:emboss;" tbl)
(puthash "bdt" "border-top:|;" tbl)
(puthash "bdw" "border-width:|;" tbl)
(puthash "pgbb" "page-break-before:|;" tbl)
(puthash "fef:eg" "font-effect:engrave;" tbl)
(puthash "bds" "border-style:|;" tbl)
(puthash "bdr" "border-right:|;" tbl)
(puthash "pgbi" "page-break-inside:|;" tbl)
(puthash "bgi" "background-image:url(|);" tbl)
(puthash "mr" "margin-right:|;" tbl)
(puthash "ta-lst" "text-align-last:|;" tbl)
(puthash "te:c" "text-emphasis:circle;" tbl)
(puthash "te:b" "text-emphasis:before;" tbl)
(puthash "q:n" "quotes:none;" tbl)
(puthash "te:n" "text-emphasis:none;" tbl)
(puthash "ta:l" "text-align:left;" tbl)
(puthash "bdbs:n" "border-bottom-style:none;" tbl)
(puthash "bt" "border-top:|;" tbl)
(puthash "bg:ie" "filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(src='${1:x}.png',sizingMethod='${2:crop}');" tbl)
(puthash "d:li" "display:list-item;" tbl)
(puthash "tj:k" "text-justify:kashida;" tbl)
(puthash "bd+" "border:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "fems:ac" "font-emphasize-style:accent;" tbl)
(puthash "fst:n" "font-stretch:normal;" tbl)
(puthash "fst:c" "font-stretch:condensed;" tbl)
(puthash "fst:e" "font-stretch:expanded;" tbl)
(puthash "wid" "widows:|;" tbl)
(puthash "bdts:n" "border-top-style:none;" tbl)
(puthash "fef" "font-effect:|;" tbl)
(puthash "cur:t" "cursor:text;" tbl)
(puthash "bdsp" "border-spacing:|;" tbl)
(puthash "cur:p" "cursor:pointer;" tbl)
(puthash "animps:r" "animation-play-state:running;" tbl)
(puthash "animps:p" "animation-play-state:paused;" tbl)
(puthash "maw:n" "max-width:none;" tbl)
(puthash "fw:n" "font-weight:normal;" tbl)
(puthash "bdtri:n" "border-top-right-image:none;" tbl)
(puthash "fw:b" "font-weight:bold;" tbl)
(puthash "bdtri:c" "border-top-right-image:continue;" tbl)
(puthash "cur:a" "cursor:auto;" tbl)
(puthash "cur:c" "cursor:crosshair;" tbl)
(puthash "bdls:n" "border-left-style:none;" tbl)
(puthash "tw:u" "text-wrap:unrestricted;" tbl)
(puthash "lts" "letter-spacing:|;" tbl)
(puthash "c:ra" "color:rgba(${1:0}, ${2:0}, ${3:0}, .${4:5});" tbl)
(puthash "va:sub" "vertical-align:sub;" tbl)
(puthash "tw:s" "text-wrap:suppress;" tbl)
(puthash "ti" "text-indent:|;" tbl)
(puthash "pgbi:av" "page-break-inside:avoid;" tbl)
(puthash "tj:t" "text-justify:tibetan;" tbl)
(puthash "bgc" "background-color:#${1:fff};" tbl)
(puthash "trf:tx" "transform: translateX(${1:x});" tbl)
(puthash "trf:ty" "transform: translateY(${1:y});" tbl)
(puthash "va:sup" "vertical-align:super;" tbl)
(puthash "va:bl" "vertical-align:baseline;" tbl)
(puthash "tw:n" "text-wrap:normal;" tbl)
(puthash "mb" "margin-bottom:|;" tbl)
(puthash "ml" "margin-left:|;" tbl)
(puthash "bdtrrs" "border-top-right-radius:|;" tbl)
(puthash "va:m" "vertical-align:middle;" tbl)
(puthash "mt" "margin-top:|;" tbl)
(puthash "bds:dt" "border-style:dotted;" tbl)
(puthash "v" "visibility:${1:hidden};" tbl)
(puthash "@import" "@import url(|);" tbl)
(puthash "ta" "text-align:${1:left};" tbl)
(puthash "pos:r" "position:relative;" tbl)
(puthash "va:b" "vertical-align:bottom;" tbl)
(puthash "tsh+" "text-shadow:${1:0} ${2:0} ${3:0} ${4:#000};" tbl)
(puthash "cps:t" "caption-side:top;" tbl)
(puthash "cps:b" "caption-side:bottom;" tbl)
(puthash "list:dclz" "list-style-type:decimal-leading-zero;" tbl)
(puthash "f+" "font:${1:1em} ${2:Arial,sans-serif};" tbl)
(puthash "animic:i" "animation-iteration-count:infinite;" tbl)
(puthash "bd:n" "border:none;" tbl)
(puthash "!" "!important" tbl)
(puthash "bdl:n" "border-left:none;" tbl)
(puthash "bgsz:a" "background-size:auto;" tbl)
(puthash "bdlen:a" "border-length:auto;" tbl)
(puthash "tw:no" "text-wrap:none;" tbl)
(puthash "bdtri" "border-top-right-image:url(|);" tbl)
(puthash "ovs" "overflow-style:${1:scrollbar};" tbl)
(puthash "mar" "max-resolution:${1:res};" tbl)
(puthash "w" "width:|;" tbl)
(puthash "maw" "max-width:|;" tbl)
(puthash "fw" "font-weight:|;" tbl)
(puthash "ovx" "overflow-x:${1:hidden};" tbl)
(puthash "ovy" "overflow-y:${1:hidden};" tbl)
(puthash "ff" "font-family:|;" tbl)
(puthash "ov" "overflow:${1:hidden};" tbl)
(puthash "wfsm:sa" "-webkit-font-smoothing:subpixel-antialiased;" tbl)
(puthash "mah" "max-height:|;" tbl)
(puthash "cnt:oq" "content:open-quote;" tbl)
(puthash "fl" "float:${1:left};" tbl)
(puthash "trsp" "transition-property:${1:prop};" tbl)
(puthash "va:tb" "vertical-align:text-bottom;" tbl)
(puthash "h:a" "height:auto;" tbl)
(puthash "d:ib" "display:inline-block;" tbl)
(puthash "bgo:cb" "background-origin:content-box;" tbl)
(puthash "q" "quotes:|;" tbl)
(puthash "tal:l" "text-align-last:left;" tbl)
(puthash "va:tt" "vertical-align:text-top;" tbl)
(puthash "fef:o" "font-effect:outline;" tbl)
(puthash "ct:ncq" "content:no-close-quote;" tbl)
(puthash "wow:nm" "word-wrap:normal;" tbl)
(puthash "ct:noq" "content:no-open-quote;" tbl)
(puthash "tov:c" "text-overflow:clip;" tbl)
(puthash "colm" "columns:|;" tbl)
(puthash "cur:d" "cursor:default;" tbl)
(puthash "bdli:n" "border-left-image:none;" tbl)
(puthash "d:tbhg" "display:table-header-group;" tbl)
(puthash "bga:s" "background-attachment:scroll;" tbl)
(puthash "tsh" "text-shadow:${1:hoff} ${2:voff} ${3:blur} ${4:#000};" tbl)
(puthash "td:n" "text-decoration:none;" tbl)
(puthash "lh" "line-height:|;" tbl)
(puthash "fst:sc" "font-stretch:semi-condensed;" tbl)
(puthash "fst:ec" "font-stretch:extra-condensed;" tbl)
(puthash "fsm:a" "font-smooth:auto;" tbl)
(puthash "bdlc:t" "border-left-color:transparent;" tbl)
(puthash "bga:f" "background-attachment:fixed;" tbl)
(puthash "fsm:n" "font-smooth:never;" tbl)
(puthash "anim" "animation:|;" tbl)
(puthash "tbl:f" "table-layout:fixed;" tbl)
(puthash "tbl:a" "table-layout:auto;" tbl)
(puthash "bdci:n" "border-corner-image:none;" tbl)
(puthash "bxsh:ra" "box-shadow:${1:inset }${2:h} ${3:v} ${4:blur} ${5:spread }rgba(${6:0}, ${7:0}, ${8:0}, .${9:5});" tbl)
(puthash "whsc:k" "white-space-collapse:keep-all;" tbl)
(puthash "bdci:c" "border-corner-image:continue;" tbl)
(puthash "va:t" "vertical-align:top;" tbl)
(puthash "colmc" "column-count:|;" tbl)
(puthash "colmf" "column-fill:|;" tbl)
(puthash "colmg" "column-gap:|;" tbl)
(puthash "bdb:n" "border-bottom:none;" tbl)
(puthash "l" "left:|;" tbl)
(puthash "lisp" "list-style-position:|;" tbl)
(puthash "colmr" "column-rule:|;" tbl)
(puthash "colms" "column-span:|;" tbl)
(puthash "colmw" "column-width:|;" tbl)
(puthash "bdbi" "border-bottom-image:url(|);" tbl)
(puthash "bdbk" "border-break:${1:close};" tbl)
(puthash "pgba:r" "page-break-after:right;" tbl)
(puthash "wfsm" "-webkit-font-smoothing:${antialiased};" tbl)
(puthash "bdbc" "border-bottom-color:#${1:000};" tbl)
(puthash "ec" "empty-cells:|;" tbl)
(puthash "te:ac" "text-emphasis:accent;" tbl)
(puthash "fs" "font-style:${italic};" tbl)
(puthash "l:a" "left:auto;" tbl)
(puthash "bdr:n" "border-right:none;" tbl)
(puthash "bdrst:n" "border-right-style:none;" tbl)
(puthash "bdbs" "border-bottom-style:|;" tbl)
(puthash "bdbw" "border-bottom-width:|;" tbl)
(puthash "whsc:ba" "white-space-collapse:break-all;" tbl)
(puthash "@kf" "@-webkit-keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}\n@-o-keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}\n@-moz-keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}\n@keyframes ${1:identifier} {\n\t${2:from} { ${3} }${6}\n\t${4:to} { ${5} }\n}" tbl)
(puthash "fv" "font-variant:|;" tbl)
(puthash "lisi" "list-style-image:|;" tbl)
(puthash "whsc:bs" "white-space-collapse:break-strict;" tbl)
(puthash "b:a" "bottom:auto;" tbl)
(puthash "bdt+" "border-top:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "fz" "font-size:|;" tbl)
(puthash "ta:c" "text-align:center;" tbl)
(puthash "bdf:sp" "border-fit:space;" tbl)
(puthash "ovy:a" "overflow-y:auto;" tbl)
(puthash "bdb+" "border-bottom:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "d:tb" "display:table;" tbl)
(puthash "bdf:st" "border-fit:stretch;" tbl)
(puthash "ti:-" "text-indent:-9999px;" tbl)
(puthash "c:r" "color:rgb(${1:0}, ${2:0}, ${3:0});" tbl)
(puthash "ta:j" "text-align:justify;" tbl)
(puthash "ovy:h" "overflow-y:hidden;" tbl)
(puthash "cnt:cs" "content:counters(|);" tbl)
(puthash "orp" "orphans:|;" tbl)
(puthash "cnt:cq" "content:close-quote;" tbl)
(puthash "ovy:s" "overflow-y:scroll;" tbl)
(puthash "ta:r" "text-align:right;" tbl)
(puthash "bdf:sc" "border-fit:scale;" tbl)
(puthash "ovy:v" "overflow-y:visible;" tbl)
(puthash "ori" "orientation:|;" tbl)
(puthash "fem" "font-emphasize:|;" tbl)
(puthash "cnt:n" "content:normal;" tbl)
(puthash "bdts" "border-top-style:|;" tbl)
(puthash "bdtw" "border-top-width:|;" tbl)
(puthash "cnt:c" "content:counter(|);" tbl)
(puthash "cnt:a" "content:attr(|);" tbl)
(puthash "bdtc" "border-top-color:#${1:000};" tbl)
(puthash "cnt:noq" "content:no-open-quote;" tbl)
(puthash "td:u" "text-decoration:underline;" tbl)
(puthash "bdti" "border-top-image:url(|);" tbl)
(puthash "bdtli" "border-top-left-image:url(|);" tbl)
(puthash "bdf:of" "border-fit:overflow;" tbl)
(puthash "fl:l" "float:left;" tbl)
(puthash "tt:n" "text-transform:none;" tbl)
(puthash "fl:n" "float:none;" tbl)
(puthash "bdbrrs" "border-bottom-right-radius:|;" tbl)
(puthash "bdf:ow" "border-fit:overwrite;" tbl)
(puthash "tt:l" "text-transform:lowercase;" tbl)
(puthash "fl:r" "float:right;" tbl)
(puthash "tt:c" "text-transform:capitalize;" tbl)
(puthash "tov" "text-overflow:${ellipsis};" tbl)
(puthash "ec:s" "empty-cells:show;" tbl)
(puthash "bgr:sp" "background-repeat:space;" tbl)
(puthash "ec:h" "empty-cells:hide;" tbl)
(puthash "tt:u" "text-transform:uppercase;" tbl)
(puthash "animdir:ar" "animation-direction:alternate-reverse;" tbl)
(puthash "bdti:n" "border-top-image:none;" tbl)
(puthash "trstf" "transition-timing-function:${1:tfunc};" tbl)
(puthash "colmrc" "column-rule-color:|;" tbl)
(puthash "fst:ee" "font-stretch:extra-expanded;" tbl)
(puthash "bdbri" "border-bottom-right-image:url(|);" tbl)
(puthash "trf:r" "transform: rotate(${1:angle});" tbl)
(puthash "colmrs" "column-rule-style:|;" tbl)
(puthash "colmrw" "column-rule-width:|;" tbl)
(puthash "rsz" "resize:|;" tbl)
(puthash "d:cp" "display:compact;" tbl)
(puthash "b" "bottom:|;" tbl)
(puthash "fef:n" "font-effect:none;" tbl)
(puthash "bdt:n" "border-top:none;" tbl)
(puthash "to+" "text-outline:${1:0} ${2:0} ${3:#000};" tbl)
(puthash "r" "right:|;" tbl)
(puthash "bdbri:n" "border-bottom-right-image:none;" tbl)
(puthash "tsh:r" "text-shadow:${1:h} ${2:v} ${3:blur} rgb(${4:0}, ${5:0}, ${6:0});" tbl)
(puthash "animdel" "animation-delay:${1:time};" tbl)
(puthash "pgbi:au" "page-break-inside:auto;" tbl)
(puthash "bdbri:c" "border-bottom-right-image:continue;" tbl)
(puthash "animic" "animation-iteration-count:${1:1};" tbl)
(puthash "tsh:n" "text-shadow:none;" tbl)
(puthash "fv:sc" "font-variant:small-caps;" tbl)
(puthash "bds:i" "border-style:inset;" tbl)
(puthash "mih" "min-height:|;" tbl)
(puthash "r:a" "right:auto;" tbl)
(puthash "bdrc:t" "border-right-color:transparent;" tbl)
(puthash "fems:ds" "font-emphasize-style:disc;" tbl)
(puthash "fems:dt" "font-emphasize-style:dot;" tbl)
(puthash "zoo" "zoom:1;" tbl)
(puthash "lis" "list-style:|;" tbl)
(puthash "mir" "min-resolution:${1:res};" tbl)
(puthash "miw" "min-width:|;" tbl)
(puthash "whs" "white-space:|;" tbl)
(puthash "bdtc:t" "border-top-color:transparent;" tbl)
(puthash "op:ms" "-ms-filter:'progid:DXImageTransform.Microsoft.Alpha(Opacity=100)';" tbl)
(puthash "bds:dtds" "border-style:dot-dash;" tbl)
(puthash "bdb" "border-bottom:|;" tbl)
(puthash "m" "margin:|;" tbl)
(puthash "bgcp:nc" "background-clip:no-clip;" tbl)
(puthash "wow" "word-wrap:|;" tbl)
(puthash "w:a" "width:auto;" tbl)
(puthash "bg:n" "background:none;" tbl)
(puthash "ml:a" "margin-left:auto;" tbl)
(puthash "bds:dtdtds" "border-style:dot-dot-dash;" tbl)
(puthash "animtf:cb" "animation-timing-function:cubic-bezier(${1:0.1}, ${2:0.7}, ${3:1.0}, ${3:0.1});" tbl)
(puthash "pgba" "page-break-after:|;" tbl)
(puthash "ff:s" "font-family:serif;" tbl)
(puthash "d:rbb" "display:ruby-base;" tbl)
(puthash "tj:d" "text-justify:distribute;" tbl)
(puthash "tj:a" "text-justify:auto;" tbl)
(puthash "trfo" "transform-origin:|;" tbl)
(puthash "ff:f" "font-family:fantasy;" tbl)
(puthash "trfs" "transform-style:${1:preserve-3d};" tbl)
(puthash "fems:c" "font-emphasize-style:circle;" tbl)
(puthash "ff:c" "font-family:cursive;" tbl)
(puthash "d:itb" "display:inline-table;" tbl)
(puthash "fems:n" "font-emphasize-style:none;" tbl)
(puthash "ff:m" "font-family:monospace;" tbl)
(puthash "pgba:au" "page-break-after:auto;" tbl)
(puthash "bdri:n" "border-right-image:none;" tbl)
(puthash "mt:a" "margin-top:auto;" tbl)
(puthash "tbl" "table-layout:|;" tbl)
(puthash "wob:l" "word-break:loose;" tbl)
(puthash "animfm:b" "animation-fill-mode:backwards;" tbl)
(puthash "animfm:f" "animation-fill-mode:forwards;" tbl)
(puthash "td:o" "text-decoration:overline;" tbl)
(puthash "td:l" "text-decoration:line-through;" tbl)
(puthash "bxz:bb" "box-sizing:border-box;" tbl)
(puthash "bxsh:n" "box-shadow:none;" tbl)
(puthash "h" "height:|;" tbl)
(puthash "pgba:al" "page-break-after:always;" tbl)
(puthash "animtf:e" "animation-timing-function:ease;" tbl)
(puthash "animtf:l" "animation-timing-function:linear;" tbl)
(puthash "bxsh:r" "box-shadow:${1:inset }${2:hoff} ${3:voff} ${4:blur} ${5:spread }rgb(${6:0}, ${7:0}, ${8:0});" tbl)
(puthash "fw:br" "font-weight:bolder;" tbl)
(puthash "ovs:p" "overflow-style:panner;" tbl)
(puthash "ovs:s" "overflow-style:scrollbar;" tbl)
(puthash "fv:n" "font-variant:normal;" tbl)
(puthash "ovs:a" "overflow-style:auto;" tbl)
(puthash "m:a" "margin:auto;" tbl)
(puthash "animdir" "animation-direction:${1:normal};" tbl)
(puthash "ovs:m" "overflow-style:move;" tbl)
(puthash "ol:n" "outline:none;" tbl)
(puthash "fsm:aw" "font-smooth:always;" tbl)
(puthash "whs:nw" "white-space:nowrap;" tbl)
(puthash "wos" "word-spacing:|;" tbl)
(puthash "wob" "word-break:|;" tbl)
(puthash "pgbb:au" "page-break-before:auto;" tbl)
(puthash "op:ie" "filter:progid:DXImageTransform.Microsoft.Alpha(Opacity=100);" tbl)
(puthash "rsz:h" "resize:horizontal;" tbl)
(puthash "bgcp" "background-clip:${1:padding-box};" tbl)
(puthash "fst:ue" "font-stretch:ultra-expanded;" tbl)
(puthash "fst:uc" "font-stretch:ultra-condensed;" tbl)
(puthash "pgbb:al" "page-break-before:always;" tbl)
(puthash "c" "color:#${1:000};" tbl)
(puthash "bdrc" "border-right-color:#${1:000};" tbl)
(puthash "bdtli:n" "border-top-left-image:none;" tbl)
(puthash "bdtli:c" "border-top-left-image:continue;" tbl)
(puthash "list:ur" "list-style-type:upper-roman;" tbl)
(puthash "fst:se" "font-stretch:semi-expanded;" tbl)
(puthash "coi" "counter-increment:|;" tbl)
(puthash "tr:n" "text-replace:none;" tbl)
(puthash "bgbk:c" "background-break:continuous;" tbl)
(puthash "ov:a" "overflow:auto;" tbl)
(puthash "te:a" "text-emphasis:after;" tbl)
(puthash "cm" "/* |${child} */" tbl)
(puthash "cl" "clear:${1:both};" tbl)
(puthash "femp" "font-emphasize-position:|;" tbl)
(puthash "ov:h" "overflow:hidden;" tbl)
(puthash "wob:bs" "word-break:break-strict;" tbl)
(puthash "ov:v" "overflow:visible;" tbl)
(puthash "ov:s" "overflow:scroll;" tbl)
(puthash "bxsh" "box-shadow:${1:inset }${2:hoff} ${3:voff} ${4:blur} ${5:color};" tbl)
(puthash "cp" "clip:|;" tbl)
(puthash "ct" "content:|;" tbl)
(puthash "pr" "padding-right:|;" tbl)
(puthash "cp:r" "clip:rect(${1:top} ${2:right} ${3:bottom} ${4:left});" tbl)
(puthash "pt" "padding-top:|;" tbl)
(puthash "ori:p" "orientation:portrait;" tbl)
(puthash "pb" "padding-bottom:|;" tbl)
(puthash "cp:a" "clip:auto;" tbl)
(puthash "bdf:c" "border-fit:clip;" tbl)
(puthash "ori:l" "orientation:landscape;" tbl)
(puthash "pl" "padding-left:|;" tbl)
(puthash "bdci" "border-corner-image:url(|);" tbl)
(puthash "bdcl" "border-collapse:|;" tbl)
(puthash "cor" "counter-reset:|;" tbl)
(puthash "bgsz:cv" "background-size:cover;" tbl)
(puthash "mb:a" "margin-bottom:auto;" tbl)
(puthash "trf:sky" "transform: skewY(${1:angle});" tbl)
(puthash "trf:skx" "transform: skewX(${1:angle});" tbl)
(puthash "d:rb" "display:ruby;" tbl)
(puthash "bdlen" "border-length:|;" tbl)
(puthash "d:ri" "display:run-in;" tbl)
(puthash "va" "vertical-align:${1:top};" tbl)
(puthash "bds:db" "border-style:double;" tbl)
(puthash "d:tbfg" "display:table-footer-group;" tbl)
(puthash "bds:ds" "border-style:dashed;" tbl)
(puthash "lis:n" "list-style:none;" tbl)
(puthash "d:b" "display:block;" tbl)
(puthash "tal:a" "text-align-last:auto;" tbl)
(puthash "tal:c" "text-align-last:center;" tbl)
(puthash "d:i" "display:inline;" tbl)
(puthash "pgba:l" "page-break-after:left;" tbl)
(puthash "bgbk:eb" "background-break:each-box;" tbl)
(puthash "d:n" "display:none;" tbl)
(puthash "tal:r" "text-align-last:right;" tbl)
(puthash "tj:iw" "text-justify:inter-word;" tbl)
(puthash "trf:sc" "transform: scale(${1:x}, ${2:y});" tbl)
(puthash "olc:i" "outline-color:invert;" tbl)
(puthash "bgr:x" "background-repeat:repeat-x;" tbl)
(puthash "bgr:y" "background-repeat:repeat-y;" tbl)
(puthash "bgr:n" "background-repeat:no-repeat;" tbl)
(puthash "cnt:ncq" "content:no-close-quote;" tbl)
(puthash "list:c" "list-style-type:circle;" tbl)
(puthash "list:d" "list-style-type:disc;" tbl)
(puthash "bdbli:c" "border-bottom-left-image:continue;" tbl)
(puthash "bdbli:n" "border-bottom-left-image:none;" tbl)
(puthash "list:n" "list-style-type:none;" tbl)
(puthash "list:s" "list-style-type:square;" tbl)
(puthash "mah:n" "max-height:none;" tbl)
(puthash "bgr:rd" "background-repeat:round;" tbl)
(puthash "th:t" "text-height:text-size;" tbl)
(puthash "d:rbtg" "display:ruby-text-group;" tbl)
(puthash "bdc:t" "border-color:transparent;" tbl)
(puthash "th:f" "text-height:font-size;" tbl)
(puthash "th:a" "text-height:auto;" tbl)
(puthash "ct:cq" "content:close-quote;" tbl)
(puthash "ct:cs" "content:counters(|);" tbl)
(puthash "th:m" "text-height:max-size;" tbl)
(puthash "animdir:a" "animation-direction:alternate;" tbl)
(puthash "bxz:cb" "box-sizing:content-box;" tbl)
(puthash "rsz:n" "resize:none;" tbl)
(puthash "cur" "cursor:${pointer};" tbl)
(puthash "whs:p" "white-space:pre;" tbl)
(puthash "rsz:b" "resize:both;" tbl)
(puthash "animdir:n" "animation-direction:normal;" tbl)
(puthash "fw:lr" "font-weight:lighter;" tbl)
(puthash "whsc:n" "white-space-collapse:normal;" tbl)
(puthash "animtf:eio" "animation-timing-function:ease-in-out;" tbl)
(puthash "animdir:r" "animation-direction:reverse;" tbl)
(puthash "te:ds" "text-emphasis:disc;" tbl)
(puthash "te:dt" "text-emphasis:dot;" tbl)
(puthash "whs:n" "white-space:normal;" tbl)
(puthash "whsc:l" "white-space-collapse:loose;" tbl)
(puthash "rsz:v" "resize:vertical;" tbl)
(puthash "wob:ba" "word-break:break-all;" tbl)
(puthash "list:dc" "list-style-type:decimal;" tbl)
(puthash "d:tbrg" "display:table-row-group;" tbl)
(puthash "tov:e" "text-overflow:ellipsis;" tbl)
(puthash "mr:a" "margin-right:auto;" tbl)
(puthash "pos" "position:${1:relative};" tbl)
(puthash "fems" "font-emphasize-style:|;" tbl)
(puthash "d" "display:${1:block};" tbl)
(puthash "@media" "@media ${1:screen} {\n\t|\n}" tbl)
(puthash "bgi:n" "background-image:none;" tbl)
(puthash "wfsm:s" "-webkit-font-smoothing:subpixel-antialiased;" tbl)
(puthash "q:en" "quotes:'\\201C' '\\201D' '\\2018' '\\2019';" tbl)
(puthash "wfsm:n" "-webkit-font-smoothing:none;" tbl)
(puthash "t" "top:|;" tbl)
(puthash "ovs:mq" "overflow-style:marquee;" tbl)
(puthash "wfsm:a" "-webkit-font-smoothing:antialiased;" tbl)
(puthash "animtf" "animation-timing-function:${1:linear};" tbl)
(puthash "bgbk" "background-break:|;" tbl)
(puthash "olo" "outline-offset:|;" tbl)
(puthash "ols" "outline-style:|;" tbl)
(puthash "olw" "outline-width:|;" tbl)
(puthash "cnt" "content:'|';" tbl)
(puthash "bg+" "background:${1:#fff} url(${2}) ${3:0} ${4:0} ${5:no-repeat};" tbl)
(puthash "bdcl:s" "border-collapse:separate;" tbl)
(puthash "bdcl:c" "border-collapse:collapse;" tbl)
(puthash "anim-" "animation:${1:name} ${2:duration} ${3:timing-function} ${4:delay} ${5:iteration-count} ${6:direction} ${7:fill-mode};" tbl)
(puthash "bgsz:ct" "background-size:contain;" tbl)
(puthash "bd" "border:|;" tbl)
(puthash "wob:n" "word-break:normal;" tbl)
(puthash "bg" "background:|;" tbl)
(puthash "wob:k" "word-break:keep-all;" tbl)
(puthash "tj:ic" "text-justify:inter-cluster;" tbl)
(puthash "bb" "border-bottom:|;" tbl)
(puthash "bl" "border-left:|;" tbl)
(puthash "tj:ii" "text-justify:inter-ideograph;" tbl)
(puthash "t:a" "top:auto;" tbl)
(puthash "bgcp:bb" "background-clip:border-box;" tbl)
(puthash "ff:ss" "font-family:sans-serif;" tbl)
(puthash "br" "border-right:|;" tbl)
(puthash "bga" "background-attachment:|;" tbl)
(puthash "ol" "outline:|;" tbl)
(puthash "cl:r" "clear:right;" tbl)
(puthash "q:ru" "quotes:'\\00AB' '\\00BB' '\\201E' '\\201C';" tbl)
(puthash "bgo" "background-origin:|;" tbl)
(puthash "bgp" "background-position:${1:0} ${2:0};" tbl)
(puthash "bgr" "background-repeat:|;" tbl)
(puthash "cl:n" "clear:none;" tbl)
(puthash "cl:l" "clear:left;" tbl)
(puthash "cl:b" "clear:both;" tbl)
(puthash "animn" "animation-name:${1:none};" tbl)
(puthash "op" "opacity:|;" tbl)
(puthash "fs:o" "font-style:oblique;" tbl)
(puthash "fs:n" "font-style:normal;" tbl)
(puthash "bds:h" "border-style:hidden;" tbl)
(puthash "bdrw" "border-right-width:|;" tbl)
(puthash "bds:n" "border-style:none;" tbl)
(puthash "bds:o" "border-style:outset;" tbl)
(puthash "fs:i" "font-style:italic;" tbl)
(puthash "ct:oq" "content:open-quote;" tbl)
(puthash "bds:g" "border-style:groove;" tbl)
(puthash "olc" "outline-color:#${1:000};" tbl)
(puthash "bds:r" "border-style:ridge;" tbl)
(puthash "bds:s" "border-style:solid;" tbl)
(puthash "bds:w" "border-style:wave;" tbl)
(puthash "z:a" "z-index:auto;" tbl)
(puthash "ct:c" "content:counter(|);" tbl)
(puthash "cur:m" "cursor:move;" tbl)
(puthash "ct:a" "content:attr(|);" tbl)
(puthash "pgbb:r" "page-break-before:right;" tbl)
(puthash "bgo:bb" "background-origin:border-box;" tbl)
(puthash "ct:n" "content:normal;" tbl)
(puthash "bdr+" "border-right:${1:1px} ${2:solid} ${3:#000};" tbl)
(puthash "pgbb:l" "page-break-before:left;" tbl)
(puthash "fza" "font-size-adjust:|;" tbl)
(puthash "d:tbcl" "display:table-column;" tbl)
(puthash "ovx:a" "overflow-x:auto;" tbl)
(puthash "whs:pl" "white-space:pre-line;" tbl)
(puthash "trsde" "transition-delay:${1:time};" tbl)
(puthash "ovx:h" "overflow-x:hidden;" tbl)
(puthash "ovx:v" "overflow-x:visible;" tbl)
(puthash "ovx:s" "overflow-x:scroll;" tbl)
(puthash "trf:scy" "transform: scaleY(${1:y});" tbl)
(puthash "trf:scx" "transform: scaleX(${1:x});" tbl)
(puthash "@f+" "@font-face {\n\tfont-family: '${1:FontName}';\n\tsrc: url('${2:FileName}.eot');\n\tsrc: url('${2:FileName}.eot?#iefix') format('embedded-opentype'),\n\t\t url('${2:FileName}.woff') format('woff'),\n\t\t url('${2:FileName}.ttf') format('truetype'),\n\t\t url('${2:FileName}.svg#${1:FontName}') format('svg');\n\tfont-style: ${3:normal};\n\tfont-weight: ${4:normal};\n}" tbl)
(puthash "trsdu" "transition-duration:${1:time};" tbl)
(puthash "d:tbcp" "display:table-caption;" tbl)
(puthash "bdrs" "border-radius:|;" tbl)
(puthash "us" "user-select:${none};" tbl)
(puthash "bgcp:pb" "background-clip:padding-box;" tbl)
(puthash "bdri" "border-right-image:url(|);" tbl)
(puthash "z" "z-index:|;" tbl)
(puthash "d:rbbg" "display:ruby-base-group;" tbl)
(puthash "tsh:ra" "text-shadow:${1:h} ${2:v} ${3:blur} rgba(${4:0}, ${5:0}, ${6:0}, .${7:5});" tbl)
(puthash "femp:a" "font-emphasize-position:after;" tbl)
(puthash "femp:b" "font-emphasize-position:before;" tbl)
(puthash "animtf:eo" "animation-timing-function:ease-out;" tbl)
(puthash "whs:pw" "white-space:pre-wrap;" tbl)
(puthash "animtf:ei" "animation-timing-function:ease-in;" tbl)
(puthash "ap" "appearance:${none};" tbl)
(puthash "animps" "animation-play-state:${1:running};" tbl)
(puthash "lisi:n" "list-style-image:none;" tbl)
(puthash "bdbc:t" "border-bottom-color:transparent;" tbl)
(puthash "pos:s" "position:static;" tbl)
(puthash "animdur" "animation-duration:${1:0}s;" tbl)
(puthash "bdi:n" "border-image:none;" tbl)
(puthash "pos:f" "position:fixed;" tbl)
(puthash "pos:a" "position:absolute;" tbl)
(puthash "fza:n" "font-size-adjust:none;" tbl)
(puthash "v:h" "visibility:hidden;" tbl)
(puthash "bgpy" "background-position-y:|;" tbl)
(puthash "bgpx" "background-position-x:|;" tbl)
(puthash "v:c" "visibility:collapse;" tbl)
(puthash "bdbli" "border-bottom-left-image:url(|);" tbl)
(puthash "fsm" "font-smooth:|;" tbl)
(puthash "animfm" "animation-fill-mode:${1:both};" tbl)
(puthash "cur:he" "cursor:help;" tbl)
(puthash "fst" "font-stretch:|;" tbl)
(puthash "cur:ha" "cursor:hand;" tbl)
(puthash "bgo:pb" "background-origin:padding-box;" tbl)
(puthash "cps" "caption-side:|;" tbl)
(puthash "v:v" "visibility:visible;" tbl)
tbl) tbl)
tbl) tbl)
tbl))
;; src/preferences.el
;; This file is generated from conf/preferences.json
;; Don't edit.
(zencoding-defparameter zencoding-preferences
(let ((tbl (make-hash-table :test 'equal)))
(puthash "html" (let ((tbl (make-hash-table :test 'equal)))
(puthash "tags" (let ((tbl (make-hash-table :test 'equal)))
(puthash "code" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "meter" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "tbody" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "font" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "noscript" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "style" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "img" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "src" "" tbl)
(puthash "alt" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "title" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "menu" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "tt" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "tr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "param" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "name" "" tbl)
(puthash "value" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "li" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "source" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "tfoot" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "th" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "input" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "type" "text" tbl)
(puthash "name" "" tbl)
(puthash "value" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "td" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "dl" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "blockquote" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "fieldset" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "big" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "dd" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "kbd" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "optgroup" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "dt" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "wbr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "button" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "summary" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "p" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "small" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "output" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "div" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "dir" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "em" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "datalist" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "frame" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "hgroup" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "meta" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "video" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "src" "" tbl)
tbl) tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "rt" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "canvas" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "rp" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "sub" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "bdo" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "dir" "" tbl)
tbl) tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "bdi" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "label" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "for" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "acronym" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "title" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "sup" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "progress" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "body" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "basefont" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "base" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "href" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "br" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "address" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "article" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "strong" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "legend" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "ol" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "script" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "caption" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "s" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "dialog" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "col" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "h2" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "h3" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "h1" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "h6" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "h4" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "h5" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "header" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "table" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "select" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "name" "" tbl)
(puthash "id" "" tbl)
tbl) tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "noframes" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "span" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "area" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "shape" "" tbl)
(puthash "href" "" tbl)
(puthash "coords" "" tbl)
(puthash "alt" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "mark" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "dfn" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "strike" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "cite" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "thead" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "head" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "option" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "value" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "form" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "action" "" tbl)
tbl) tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "hr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "var" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "link" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "href" "" tbl)
(puthash "rel" "stylesheet" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "ruby" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "b" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "colgroup" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "keygen" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "ul" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "applet" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "del" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "iframe" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "src" "" tbl)
(puthash "frameborder" "0" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "embed" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "src" "" tbl)
(puthash "type" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "pre" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "frameset" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "figure" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "ins" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "aside" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "html" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "nav" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "details" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "u" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "samp" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "map" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "name" "" tbl)
tbl) tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "track" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" t tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "object" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "type" "" tbl)
(puthash "data" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "figcaption" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "a" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "href" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "center" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "textarea" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "rows" "10" tbl)
(puthash "cols" "30" tbl)
(puthash "name" "" tbl)
(puthash "id" "" tbl)
tbl) tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "footer" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "i" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "q" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "command" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "time" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" nil tbl)
tbl) tbl)
(puthash "audio" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "src" "" tbl)
tbl) tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "section" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "block" t tbl)
tbl) tbl)
(puthash "abbr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "selfClosing" nil tbl)
(puthash "defaultAttr" (let ((tbl (make-hash-table :test 'equal)))
(puthash "title" "" tbl)
tbl) tbl)
(puthash "block" nil tbl)
tbl) tbl)
tbl) tbl)
tbl) tbl)
(puthash "css" (let ((tbl (make-hash-table :test 'equal)))
(puthash "vendorPrefixesProperties" (let ((tbl (make-hash-table :test 'equal)))
(puthash "text-fill-color" (vector 
"webkit"
)
 tbl)
(puthash "ime-mode" (vector 
"ms"
)
 tbl)
(puthash "grid-layer" (vector 
"ms"
)
 tbl)
(puthash "user-drag" (vector 
"webkit"
)
 tbl)
(puthash "mask-image" (vector 
"webkit"
)
 tbl)
(puthash "font-feature-settings" (vector 
"moz"
"ms"
)
 tbl)
(puthash "outline-radius-topleft" (vector 
"moz"
)
 tbl)
(puthash "mask-box-image-slice" (vector 
"webkit"
)
 tbl)
(puthash "background-composite" (vector 
"webkit"
)
 tbl)
(puthash "text-underline-position" (vector 
"ms"
)
 tbl)
(puthash "hyphenate-limit-chars" (vector 
"ms"
)
 tbl)
(puthash "marquee-speed" (vector 
"o"
)
 tbl)
(puthash "input-required" (vector 
"o"
)
 tbl)
(puthash "color-correction" (vector 
"webkit"
)
 tbl)
(puthash "background-size" (vector 
"webkit"
)
 tbl)
(puthash "background-origin" (vector 
"webkit"
)
 tbl)
(puthash "wrap-through" (vector 
"ms"
)
 tbl)
(puthash "text-emphasis-position" (vector 
"webkit"
)
 tbl)
(puthash "user-select" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "dashboard-region" (vector 
"webkit"
"o"
)
 tbl)
(puthash "accesskey" (vector 
"o"
)
 tbl)
(puthash "scrollbar-track-color" (vector 
"ms"
)
 tbl)
(puthash "grid-columns" (vector 
"ms"
)
 tbl)
(puthash "text-stroke-width" (vector 
"webkit"
)
 tbl)
(puthash "link-source" (vector 
"o"
)
 tbl)
(puthash "scrollbar-arrow-color" (vector 
"ms"
)
 tbl)
(puthash "border-radius" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "text-stroke-color" (vector 
"webkit"
)
 tbl)
(puthash "box-shadow" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "flow-into" (vector 
"ms"
)
 tbl)
(puthash "scrollbar-highlight-color" (vector 
"ms"
)
 tbl)
(puthash "border-left-colors" (vector 
"moz"
)
 tbl)
(puthash "word-wrap" (vector 
"ms"
)
 tbl)
(puthash "text-emphasis-color" (vector 
"webkit"
)
 tbl)
(puthash "hyphens" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "marquee-direction" (vector 
"webkit"
)
 tbl)
(puthash "column-rule-width" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "box-lines" (vector 
"webkit"
"ms"
)
 tbl)
(puthash "column-break-inside" (vector 
"webkit"
)
 tbl)
(puthash "scroll-snap-points-x" (vector 
"ms"
)
 tbl)
(puthash "grid-row-align" (vector 
"ms"
)
 tbl)
(puthash "force-broken-image-icon" (vector 
"moz"
)
 tbl)
(puthash "animation-fill-mode" (vector 
"webkit"
"moz"
"o"
)
 tbl)
(puthash "box-line-progression" (vector 
"ms"
)
 tbl)
(puthash "mask-size" (vector 
"webkit"
)
 tbl)
(puthash "scroll-boundary-left" (vector 
"ms"
)
 tbl)
(puthash "text-combine" (vector 
"webkit"
)
 tbl)
(puthash "grid-column-align" (vector 
"ms"
)
 tbl)
(puthash "perspective-origin" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "overflow-style" (vector 
"ms"
)
 tbl)
(puthash "mask-position" (vector 
"webkit"
)
 tbl)
(puthash "content-zoom-chaining" (vector 
"ms"
)
 tbl)
(puthash "column-span" (vector 
"webkit"
)
 tbl)
(puthash "border-fit" (vector 
"webkit"
)
 tbl)
(puthash "outline-radius-topright" (vector 
"moz"
)
 tbl)
(puthash "tab-size" (vector 
"moz"
"o"
)
 tbl)
(puthash "layout-grid-type" (vector 
"ms"
)
 tbl)
(puthash "mask-clip" (vector 
"webkit"
)
 tbl)
(puthash "column-gap" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "text-security" (vector 
"webkit"
)
 tbl)
(puthash "text-align-last" (vector 
"ms"
)
 tbl)
(puthash "svg-shadow" (vector 
"webkit"
)
 tbl)
(puthash "accelerator" (vector 
"ms"
)
 tbl)
(puthash "locale" (vector 
"webkit"
)
 tbl)
(puthash "line-clamp" (vector 
"webkit"
)
 tbl)
(puthash "column-count" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "line-box-contain" (vector 
"webkit"
)
 tbl)
(puthash "line-break" (vector 
"webkit"
"ms"
)
 tbl)
(puthash "transform-style" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "perspective" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "orient" (vector 
"moz"
)
 tbl)
(puthash "float-edge" (vector 
"moz"
)
 tbl)
(puthash "image-region" (vector 
"moz"
)
 tbl)
(puthash "mask-box-image-width" (vector 
"webkit"
)
 tbl)
(puthash "background-inline-policy" (vector 
"moz"
)
 tbl)
(puthash "user-modify" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "content-zoom-snap-type" (vector 
"ms"
)
 tbl)
(puthash "outline-radius-bottomright" (vector 
"moz"
)
 tbl)
(puthash "wrap-margin" (vector 
"ms"
)
 tbl)
(puthash "transform-origin-z" (vector 
"ms"
)
 tbl)
(puthash "mask-box-image" (vector 
"webkit"
)
 tbl)
(puthash "scroll-chaining" (vector 
"ms"
)
 tbl)
(puthash "grid-row" (vector 
"ms"
)
 tbl)
(puthash "nbsp-mode" (vector 
"webkit"
)
 tbl)
(puthash "mask-box-image-source" (vector 
"webkit"
)
 tbl)
(puthash "font-language-override" (vector 
"moz"
)
 tbl)
(puthash "animation-delay" (vector 
"webkit"
"moz"
"o"
)
 tbl)
(puthash "hyphenate-character" (vector 
"webkit"
)
 tbl)
(puthash "box-flex-group" (vector 
"webkit"
)
 tbl)
(puthash "border-image" (vector 
"webkit"
"moz"
"o"
)
 tbl)
(puthash "highlight" (vector 
"webkit"
)
 tbl)
(puthash "marquee-repetition" (vector 
"webkit"
)
 tbl)
(puthash "grid-column" (vector 
"ms"
)
 tbl)
(puthash "box-align" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "user-input" (vector 
"moz"
)
 tbl)
(puthash "animation-duration" (vector 
"webkit"
"moz"
"o"
)
 tbl)
(puthash "transform-origin-y" (vector 
"ms"
)
 tbl)
(puthash "box-ordinal-group" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "box-orient" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "stack-sizing" (vector 
"moz"
)
 tbl)
(puthash "transition" (vector 
"webkit"
"moz"
"ms"
"o"
)
 tbl)
(puthash "text-size-adjust" (vector 
"moz"
"ms"
)
 tbl)
(puthash "filter" (vector 
"ms"
)
 tbl)
(puthash "content-zoom-boundary" (vector 
"ms"
)
 tbl)
(puthash "layout-grid-char" (vector 
"ms"
)
 tbl)
(puthash "perspective-origin-x" (vector 
"ms"
)
 tbl)
(puthash "perspective-origin-y" (vector 
"ms"
)
 tbl)
(puthash "column-rule-color" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "high-contrast-adjust" (vector 
"ms"
)
 tbl)
(puthash "text-justify" (vector 
"ms"
)
 tbl)
(puthash "text-decoration-color" (vector 
"moz"
)
 tbl)
(puthash "block-progression" (vector 
"ms"
)
 tbl)
(puthash "border-horizontal-spacing" (vector 
"webkit"
)
 tbl)
(puthash "wrap-flow" (vector 
"ms"
)
 tbl)
(puthash "margin-before-collapse" (vector 
"webkit"
)
 tbl)
(puthash "table-baseline" (vector 
"o"
)
 tbl)
(puthash "background-clip" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "interpolation-mode" (vector 
"ms"
)
 tbl)
(puthash "scroll-snap-type" (vector 
"ms"
)
 tbl)
(puthash "text-emphasis-style" (vector 
"webkit"
)
 tbl)
(puthash "flow-from" (vector 
"ms"
)
 tbl)
(puthash "backface-visibility" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "transform" (vector 
"webkit"
"moz"
"ms"
"o"
)
 tbl)
(puthash "column-break-after" (vector 
"webkit"
)
 tbl)
(puthash "mask-composite" (vector 
"webkit"
)
 tbl)
(puthash "layout-grid" (vector 
"ms"
)
 tbl)
(puthash "column-width" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "scrollbar-base-color" (vector 
"ms"
)
 tbl)
(puthash "window-shadow" (vector 
"moz"
)
 tbl)
(puthash "object-fit" (vector 
"o"
)
 tbl)
(puthash "text-decorations-in-effect" (vector 
"webkit"
)
 tbl)
(puthash "box-flex" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "hyphenate-limit-after" (vector 
"webkit"
)
 tbl)
(puthash "animation-play-state" (vector 
"webkit"
"moz"
"o"
)
 tbl)
(puthash "transition-delay" (vector 
"webkit"
"moz"
"ms"
"o"
)
 tbl)
(puthash "input-format" (vector 
"o"
)
 tbl)
(puthash "mask-attachment" (vector 
"webkit"
)
 tbl)
(puthash "scroll-boundary-right" (vector 
"ms"
)
 tbl)
(puthash "animation-iteration-count" (vector 
"webkit"
"moz"
"o"
)
 tbl)
(puthash "link" (vector 
"o"
)
 tbl)
(puthash "content-zoom-boundary-max" (vector 
"ms"
)
 tbl)
(puthash "text-decoration-style" (vector 
"moz"
)
 tbl)
(puthash "text-autospace" (vector 
"ms"
)
 tbl)
(puthash "mask-origin" (vector 
"webkit"
)
 tbl)
(puthash "user-focus" (vector 
"moz"
)
 tbl)
(puthash "border-bottom-colors" (vector 
"moz"
)
 tbl)
(puthash "scroll-boundary-top" (vector 
"ms"
)
 tbl)
(puthash "content-zooming" (vector 
"ms"
)
 tbl)
(puthash "appearance" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "scroll-snap-x" (vector 
"ms"
)
 tbl)
(puthash "marquee-loop" (vector 
"o"
)
 tbl)
(puthash "column-rule-style" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "marquee-increment" (vector 
"webkit"
)
 tbl)
(puthash "behavior" (vector 
"ms"
)
 tbl)
(puthash "box-pack" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "animation-direction" (vector 
"webkit"
"moz"
"o"
)
 tbl)
(puthash "scrollbar-darkshadow-color" (vector 
"ms"
)
 tbl)
(puthash "animation-timing-function" (vector 
"webkit"
"moz"
"o"
)
 tbl)
(puthash "text-blink" (vector 
"moz"
)
 tbl)
(puthash "scroll-boundary-bottom" (vector 
"ms"
)
 tbl)
(puthash "mask-box-image-repeat" (vector 
"webkit"
)
 tbl)
(puthash "text-orientation" (vector 
"webkit"
)
 tbl)
(puthash "grid-column-span" (vector 
"ms"
)
 tbl)
(puthash "binding" (vector 
"moz"
)
 tbl)
(puthash "transform-origin" (vector 
"webkit"
"moz"
"ms"
"o"
)
 tbl)
(puthash "text-decoration-line" (vector 
"moz"
)
 tbl)
(puthash "animation-name" (vector 
"webkit"
"moz"
"o"
)
 tbl)
(puthash "marquee-dir" (vector 
"o"
)
 tbl)
(puthash "content-zoom-snap" (vector 
"ms"
)
 tbl)
(puthash "border-top-colors" (vector 
"moz"
)
 tbl)
(puthash "border-vertical-spacing" (vector 
"webkit"
)
 tbl)
(puthash "grid-rows" (vector 
"ms"
)
 tbl)
(puthash "box-reflect" (vector 
"webkit"
)
 tbl)
(puthash "hyphenate-limit-before" (vector 
"webkit"
)
 tbl)
(puthash "mask-box-image-outset" (vector 
"webkit"
)
 tbl)
(puthash "outline-radius-bottomleft" (vector 
"moz"
)
 tbl)
(puthash "text-overflow" (vector 
"ms"
)
 tbl)
(puthash "border-right-colors" (vector 
"moz"
)
 tbl)
(puthash "layout-flow" (vector 
"ms"
)
 tbl)
(puthash "box-direction" (vector 
"webkit"
"moz"
"ms"
)
 tbl)
(puthash "animation" (vector 
"webkit"
"o"
)
 tbl)
(puthash "hyphenate-limit-lines" (vector 
"ms"
)
 tbl)
(puthash "scrollbar-face-color" (vector 
"ms"
)
 tbl)
(puthash "column-break-before" (vector 
"webkit"
)
 tbl)
(puthash "content-zoom-snap-points" (vector 
"ms"
)
 tbl)
(puthash "grid-row-span" (vector 
"ms"
)
 tbl)
(puthash "touch-action" (vector 
"ms"
)
 tbl)
(puthash "marquee-style" (vector 
"webkit"
"o"
)
 tbl)
(puthash "margin-after-collapse" (vector 
"webkit"
)
 tbl)
(puthash "text-kashida-space" (vector 
"ms"
)
 tbl)
(puthash "scrollbar-shadow-color" (vector 
"ms"
)
 tbl)
(puthash "object-position" (vector 
"o"
)
 tbl)
(puthash "hyphenate-limit-zone" (vector 
"ms"
)
 tbl)
(puthash "scroll-snap-y" (vector 
"ms"
)
 tbl)
(puthash "transform-origin-x" (vector 
"ms"
)
 tbl)
(puthash "transition-property" (vector 
"webkit"
"moz"
"ms"
"o"
)
 tbl)
(puthash "word-break" (vector 
"ms"
)
 tbl)
(puthash "scroll-snap-points-y" (vector 
"ms"
)
 tbl)
(puthash "scroll-boundary" (vector 
"ms"
)
 tbl)
(puthash "mask-repeat" (vector 
"webkit"
)
 tbl)
(puthash "writing-mode" (vector 
"webkit"
"ms"
)
 tbl)
(puthash "layout-grid-mode" (vector 
"ms"
)
 tbl)
(puthash "background-position-y" (vector 
"ms"
)
 tbl)
(puthash "scroll-rails" (vector 
"ms"
)
 tbl)
(puthash "content-zoom-boundary-min" (vector 
"ms"
)
 tbl)
(puthash "background-position-x" (vector 
"ms"
)
 tbl)
(puthash "box-sizing" (vector 
"webkit"
"moz"
)
 tbl)
(puthash "transition-duration" (vector 
"webkit"
"moz"
"ms"
"o"
)
 tbl)
(puthash "rtl-ordering" (vector 
"webkit"
)
 tbl)
(puthash "layout-grid-line" (vector 
"ms"
)
 tbl)
(puthash "font-smoothing" (vector 
"webkit"
)
 tbl)
(puthash "transition-timing-function" (vector 
"webkit"
"moz"
"ms"
"o"
)
 tbl)
tbl) tbl)
(puthash "unitlessProperties" (vector 
"z-index"
"line-height"
"opacity"
"font-weight"
"zoom"
)
 tbl)
(puthash "floatUnit" "em" tbl)
(puthash "intUnit" "px" tbl)
(puthash "keywordAliases" (let ((tbl (make-hash-table :test 'equal)))
(puthash "a" "auto" tbl)
(puthash "do" "dotted" tbl)
(puthash "i" "inherit" tbl)
(puthash "da" "dashed" tbl)
(puthash "s" "solid" tbl)
(puthash "t" "transparent" tbl)
tbl) tbl)
(puthash "keywords" (vector 
"auto"
"inherit"
)
 tbl)
(puthash "unitAliases" (let ((tbl (make-hash-table :test 'equal)))
(puthash "p" "%" tbl)
(puthash "r" "rem" tbl)
(puthash "e" "em" tbl)
(puthash "x" "ex" tbl)
tbl) tbl)
tbl) tbl)
tbl))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML abbrev

(zencoding-defparameter
 zencoding-tag-aliases-table
 (gethash "aliases" (gethash "html" zencoding-snippets)))

(defun zencoding-expr (input)
  "Parse a zen coding expression with optional filters."
  (zencoding-pif (zencoding-parse "\\(.*?\\)|" 2 "expr|filter" it)
                 (let ((input (elt it 1))
                       (filters (elt it 2)))
                   (zencoding-pif (zencoding-extract-filters filters)
                                  (zencoding-filter input it)
                                  it))
                 (zencoding-filter input (zencoding-default-filter))))

(defun zencoding-subexpr (input)
  "Parse a zen coding expression with no filter. This pretty much defines precedence."
  (zencoding-run zencoding-siblings
                 it
                 (zencoding-run zencoding-parent-child
                                it
                                (zencoding-run zencoding-multiplier
                                               it
                                               (zencoding-run zencoding-pexpr
                                                              it
                                                              (zencoding-run zencoding-text
                                                                             it
                                                                             (zencoding-run zencoding-tag
                                                                                            it
                                                                                            '(error "no match, expecting ( or a-zA-Z0-9"))))))))

(defun zencoding-extract-filters (input)
  "Extract filters from expression."
  (zencoding-pif (zencoding-parse "\\([^\\|]+?\\)|" 2 "" it)
                 (let ((filter-name (elt it 1))
                       (more-filters (elt it 2)))
                   (zencoding-pif (zencoding-extract-filters more-filters)
                                  (cons filter-name it)
                                  it))
                 (zencoding-parse "\\([^\\|]+\\)" 1 "filter name" `(,(elt it 1)))))

(defun zencoding-filter (input filters)
  "Construct AST with specified filters."
  (zencoding-pif (zencoding-subexpr input)
                 (let ((result (car it))
                       (rest (cdr it)))
                   `((filter ,filters ,result) . ,rest))
                 it))

(defun zencoding-default-filter ()
  "Default filter(s) to be used if none is specified."
  (let* ((file-ext (car (zencoding-regex ".*\\(\\..*\\)" (or (buffer-file-name) "") 1)))
         (defaults '(".html" ("html")
                     ".htm"  ("html")
                     ".haml" ("haml")
                     ".clj"  ("hic")))
         (default-else      '("html"))
         (selected-default (member file-ext defaults)))
    (if selected-default
        (cadr selected-default)
      default-else)))

(defun zencoding-numbering (input)
  (zencoding-parse
   "\\(\\$+\\)" 2 "numbering, $"
   (let ((doller (elt it 1)))
     (zencoding-pif (zencoding-parse
                     "@\\([0-9-][0-9]*\\)" 2 "numbering args"
                     (let* ((args (read (elt it 1)))
                            (direction  (not (or (eq '- args) (minusp args))))
                            (base       (if (eq '- args) 1 (abs args))))
                       `((n ,(length doller) ,direction ,base) . ,input)))
                    it
                    `((n ,(length doller) t 1) . ,input)))))

(defun zencoding-split-numbering-expressions (input)
  (labels
      ((iter (input)
             (zencoding-aif (zencoding-regex "\\([^$]*\\)\\(\\$.*\\)" input '(1 2))
                (let ((prefix (car it))
                      (input (cadr it)))
                  (if (and (< 0 (length prefix)) ; check if ..\\$... or ...$...
                           (string-equal (substring prefix -1) "\\"))
                      `(,(store-substring prefix (- (length prefix) 1) ?$)
                        ,@(iter (substring input 1)))
                    (let ((res (zencoding-numbering input)))
                      `(,prefix ,(car res) ,@(iter (cdr res))))))
                (list input))))
    (let ((res (iter input)))
      (if (every #'stringp res)
          (apply #'concat res)
        `(numberings ,@res)))))

(defun zencoding-instantiate-numbering-expression (i lim exp)
  (labels ((instantiate
            (i lim exps)
            (apply #'concat
                   (mapcar
                    (lambda (exp)
                      (if (listp exp)
                          (let ((digits (second exp))
                                (direction (third exp))
                                (base (fourth exp)))
                            (let ((num (if direction (+ base i)
                                         (- (+ lim (- base 1)) i))))
                              (format (concat "%0" (format "%d" digits) "d") num)))
                        exp)) exps)))
           (search
            (i lim exp)
            (if (listp exp)
                (if (eql (car exp) 'numberings)
                    (instantiate i lim (cdr exp))
                  ;; Should do like this for real searching.
                  ;; But stack overflow occurs.
                  ;; (cons (search-numberings i lim (car exp))
                  ;;       (search-numberings i lim (cdr exp)))
                  (mapcar (lambda (exp) (search i lim exp)) exp))
              exp)))
    (search i lim exp)))

(defun zencoding-multiply-expression (multiplicand exp)
  (loop for i to (- multiplicand 1) collect
        (zencoding-instantiate-numbering-expression i multiplicand exp)))

(defun zencoding-multiplier (input)
  (zencoding-pif (zencoding-run zencoding-pexpr
                                it
                                (zencoding-run zencoding-tag
                                               it
                                               (zencoding-run zencoding-text
                                                              it
                                                              '(error "expected *n multiplier"))))
                 (let* ((expr (car it)) (input (cdr it))
                        (multiplier expr))
                   (zencoding-parse "\\*\\([0-9]+\\)" 2 "*n where n is a number"
                                    (let ((multiplicand (read (elt it 1))))
                                      `((list ,(zencoding-multiply-expression
                                                multiplicand
                                                multiplier)) . ,input))))))

(defun zencoding-tag (input)
  "Parse a tag."
  (zencoding-run
   zencoding-tagname
   (let ((tagname (cadr expr))
         (has-body? (cddr expr)))
     (zencoding-pif
      (zencoding-run zencoding-identifier
                     (zencoding-tag-classes
                      `(tag (,tagname ,has-body? ,(cddr expr))) input)
                     (zencoding-tag-classes
                      `(tag (,tagname ,has-body? nil)) input))
      (let ((tag-data (cadar it)) (input (cdr it)))
        (zencoding-pif (zencoding-run
                        zencoding-props
                        (let ((props (cdr expr)))
                          `((tag ,(append tag-data (list props))) . ,input))
                        `((tag ,(append tag-data '(nil))) . ,input))
                       (let ((expr (car it)) (input (cdr it)))
                         (destructuring-bind (expr . input)
                             (zencoding-tag-text expr input)
                           (zencoding-expand-tag-alias expr input)))))))
   (zencoding-default-tag input)))

(defun zencoding-get-first-tag (expr)
  (if (listp expr)
      (if (listp (car expr))
          (zencoding-get-first-tag (car expr))
        (if (eql (car expr) 'tag)
            expr
          (zencoding-get-first-tag (cdr expr))))
    nil))

(defun zencoding-expand-tag-alias (tag input)
  (let ((tag-data (cadr tag)))
    (let ((tag-name (car tag-data)))
      (zencoding-aif
       (gethash tag-name zencoding-tag-aliases-table)
       (let ((expr (if (stringp it)
                       (zencoding-subexpr it)
                     it)))
         (prog1
             (let ((rt (copy-tree expr)))
               (let ((first-tag-data (cadr (zencoding-get-first-tag rt))))
                 (setf (second first-tag-data) (second tag-data))
                 (setf (third first-tag-data)  (third tag-data))
                 (setf (fourth first-tag-data)
                       (remove-duplicates
                        (append (fourth first-tag-data)
                                (fourth tag-data)) :test #'string=))
                 (setf (fifth first-tag-data)
                       (remove-duplicates
                        (append (fifth first-tag-data)
                                (fifth tag-data))
                        :test #'(lambda (p1 p2)
                                  (eql (car p1) (car p2)))))
                 (setf (sixth first-tag-data) (sixth tag-data))
                 (setf (cdr rt) (concat (cdr rt) input))
                 rt))
           (puthash tag-name expr zencoding-tag-aliases-table)))
       `(,tag . ,input)))))

(defun zencoding-default-tag (input)
  "Parse a #id or .class"
  (zencoding-parse "\\([#|\\.]\\)" 1 "tagname"
                   (zencoding-tag (concat "div" (elt it 0)))))

(defun zencoding-tag-text (tag input)
  (let ((tag-data (cadr tag)))
    (zencoding-run zencoding-text
                   (let ((txt (cadr expr)))
                     `((tag ,(append tag-data (list txt))) . ,input))
                   `((tag ,(append tag-data '(nil))) . ,input))))

(defun zencoding-tag-props (tag input)
  (let ((tag-data (cadr tag)))
    (zencoding-run zencoding-props
                   (let ((props (cdr expr)))
                     `((tag ,(append tag-data (list props))) . ,input))
                   `((tag ,(append tag-data '(nil))) . ,input))))

(defun zencoding-props (input)
  "Parse many props."
    (zencoding-run zencoding-prop
                   (zencoding-pif (zencoding-props input)
                                  `((props . ,(cons expr (cdar it))) . ,(cdr it))
                                  `((props . ,(list expr)) . ,input))))

(defun zencoding-prop (input)
  (zencoding-parse
   " " 1 "space"
   (zencoding-run
    zencoding-name
    (let ((name (cdr expr)))
      (zencoding-pif (zencoding-prop-value name input)
                     it
                     `((,(read name) "") . ,input))))))

(defun zencoding-prop-value (name input)
  (zencoding-pif (zencoding-parse "=\"\\(.*?\\)\"" 2
                                  "=\"property value\""
                                  (let ((value (elt it 1))
                                        (input (elt it 2)))
                                    `((,(read name) ,value) . ,input)))
                 it
                 (zencoding-parse "=\\([^\\,\\+\\>\\ )]*\\)" 2
                                  "=property value"
                                  (let ((value (elt it 1))
                                        (input (elt it 2)))
                                    `((,(read name) ,value) . ,input)))))

(defun zencoding-tag-classes (tag input)
  (let ((tag-data (cadr tag)))
    (zencoding-run zencoding-classes
                   (let ((classes (mapcar (lambda (cls) (cdadr cls))
                                          (cdr expr))))
                     `((tag ,(append tag-data (list classes))) . ,input))
                   `((tag ,(append tag-data '(nil))) . ,input))))

(defun zencoding-tagname (input)
  "Parse a tagname a-zA-Z0-9 tagname (e.g. html/head/xsl:if/br)."
  (zencoding-parse "\\([a-zA-Z!][a-zA-Z0-9:!$@-]*\/?\\)" 2 "tagname, a-zA-Z0-9"
                   (let* ((tag-spec (elt it 1))
                          (empty-tag (zencoding-regex "\\([^\/]*\\)\/" tag-spec 1))
                          (tag (zencoding-split-numbering-expressions
                                (if empty-tag (car empty-tag) tag-spec))))
                     `((tagname . (,tag . ,(not empty-tag))) . ,input))))

(defun zencoding-text (input)
  "A zen coding expression innertext."
  (zencoding-parse "{\\(.*?\\)}" 2 "inner text"
                   (let ((txt (zencoding-split-numbering-expressions (elt it 1))))
                     `((text ,txt) . ,input))))

(defun zencoding-pexpr (input)
  "A zen coding expression with parentheses around it."
  (zencoding-parse "(" 1 "("
                   (zencoding-run zencoding-subexpr
                                  (zencoding-aif (zencoding-regex ")" input '(0 1))
                                                 `(,expr . ,(elt it 1))
                                                 '(error "expecting `)'")))))

(defun zencoding-parent-child (input)
  "Parse an tag>e expression, where `n' is an tag and `e' is any
   expression."
  (defun listing (parents child input)
    (let ((len (length parents)))
      `((list ,(map 'list
                    (lambda (parent i)
                      `(parent-child ,parent
                                     ,(zencoding-instantiate-numbering-expression i len child)))
                    parents
                    (loop for i to (- len 1) collect i))) . ,input)))
  (zencoding-run zencoding-multiplier
                 (let* ((items (cadr expr))
                        (rest (zencoding-child-sans expr input)))
                   (if (not (eq (car rest) 'error))
                       (let ((child (car rest))
                             (input (cdr rest)))

                         (zencoding-aif (zencoding-regex "^" input '(0 1))
                                                   (let ((input (elt it 1)))
                                                     (zencoding-run zencoding-subexpr
                                                                    `((sibling ,(car (listing items child "")) ,expr) . ,input)
                                                                    (listing items child input)))
                                                   (listing items child input)))
                     '(error "expected child")))
                 (zencoding-run zencoding-tag
                                (zencoding-child expr input)
                                '(error "expected parent"))))

(defun zencoding-child-sans (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-subexpr
                                  it
                                  '(error "expected child"))))

(defun zencoding-child (parent input)
  (zencoding-parse ">" 1 ">"
                   (zencoding-run zencoding-subexpr
                                  (let ((child expr))
                                    (zencoding-aif (zencoding-regex "^" input '(0 1))
                                                   (let ((input (elt it 1)))
                                                     (zencoding-run zencoding-subexpr
                                                                    `((sibling (parent-child ,parent ,child) ,expr) . ,input)
                                                                    `((parent-child ,parent ,child) . ,input)))
                                                   `((parent-child ,parent ,child) . ,input)))
                                  '(error "expected child"))))

(defun zencoding-sibling (input)
  (zencoding-por zencoding-pexpr zencoding-multiplier
                 it
                 (zencoding-run zencoding-tag
                                it
                                (zencoding-run zencoding-text
                                               it
                                               '(error "expected sibling")))))

(defun zencoding-siblings (input)
  "Parse an e+e expression, where e is an tag or a pexpr."
  (zencoding-run zencoding-sibling
                 (let ((parent expr))
                   (zencoding-parse
                    "\\+" 1 "+"
                    (zencoding-run
                     zencoding-subexpr
                     (let ((child expr))
                       `((sibling ,parent ,child) . ,input))
                     (zencoding-expand parent input))))
                 '(error "expected first sibling")))

(defun zencoding-expand (parent input)
  "Parse an e+ expression, where e is an expandable tag"
  (let* ((parent-tag (car (cadr parent))))
    (setf (caadr parent) (concat parent-tag "+"))
    (destructuring-bind (parent . input)
        (zencoding-expand-tag-alias parent input)
      (zencoding-pif (zencoding-parse "+\\(.*\\)" 1 "+expr"
                                      (zencoding-subexpr (elt it 1)))
                     `((sibling ,parent ,@it))
                     `(,parent . ,input)))))

(defun zencoding-name (input)
  "Parse a class or identifier name, e.g. news, footer, mainimage"
  (zencoding-parse "\\([a-zA-Z$@][a-zA-Z0-9$@_:-]*\\)" 2 "class or identifer name"
                   `((name . ,(zencoding-split-numbering-expressions
                               (elt it 1))) . ,input)))

(defun zencoding-class (input)
  "Parse a classname expression, e.g. .foo"
  (zencoding-parse "\\." 1 "."
                   (zencoding-run zencoding-name
                                  `((class ,expr) . ,input)
                                  '(error "expected class name"))))
(defun zencoding-identifier (input)
  "Parse an identifier expression, e.g. #foo"
  (zencoding-parse "#" 1 "#"
                   (zencoding-run zencoding-name
                                  `((identifier . ,expr) . ,input))))

(defun zencoding-classes (input)
  "Parse many classes."
  (zencoding-run zencoding-class
                 (zencoding-pif (zencoding-classes input)
                                `((classes . ,(cons expr (cdar it))) . ,(cdr it))
                                `((classes . ,(list expr)) . ,input))
                 '(error "expected class")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zen coding transformer from AST to string

(defvar zencoding-leaf-function nil
  "Function to execute when expanding a leaf node in the
  Zencoding AST.")

(zencoding-defparameter
 zencoding-tag-settings-table
 (gethash "tags" (gethash "html" zencoding-preferences)))

(zencoding-defparameter
 zencoding-tag-snippets-table
 (gethash "snippets" (gethash "html" zencoding-snippets)))

(defvar zencoding-filters
  '("html" (zencoding-primary-filter zencoding-make-html-tag)
    "c"    (zencoding-primary-filter zencoding-make-commented-html-tag)
    "haml" (zencoding-primary-filter zencoding-make-haml-tag)
    "hic"  (zencoding-primary-filter zencoding-make-hiccup-tag)
    "e"    (zencoding-escape-xml)))

(defun zencoding-primary-filter (input proc)
  "Process filter that needs to be executed first, ie. not given output from other filter."
  (if (listp input)
      (let ((tag-maker (cadr proc)))
        (zencoding-transform-ast input tag-maker))
    nil))

(defun zencoding-process-filter (filters input)
  "Process filters, chain one filter output as the input of the next filter."
  (let ((filter-data (member (car filters) zencoding-filters))
        (more-filters (cdr filters)))
    (if filter-data
        (let* ((proc   (cadr filter-data))
               (fun    (car proc))
               (filter-output (funcall fun input proc)))
          (if more-filters
              (zencoding-process-filter more-filters filter-output)
            filter-output))
      nil)))

(defun zencoding-make-tag (tag-maker tag-info &optional content)
  "Extract tag info and pass them to tag-maker."
  (let* ((name      (pop tag-info))
         (has-body? (pop tag-info))
         (id        (pop tag-info))
         (classes   (pop tag-info))
         (props     (pop tag-info))
         (txt       (pop tag-info))
         (settings  (gethash name zencoding-tag-settings-table)))
    (funcall tag-maker name has-body? id classes props txt settings
             (if content content
               (if zencoding-leaf-function (funcall zencoding-leaf-function))))))

(defun zencoding-hash-to-list (hash &optional proc)
  (unless proc (setq proc #'cons))
  (loop for key being the hash-keys of hash using (hash-values val)
        collect (funcall proc key val)))

(defun zencoding-merge-tag-props (default-table tag-props)
  (if default-table
      (let ((tbl (copy-hash-table default-table)))
        (loop for prop in tag-props do
              (puthash (symbol-name (car prop)) (cadr prop) tbl))
        (zencoding-hash-to-list tbl 'list))
    tag-props))

(defun zencoding-html-snippets-instantiate-lambda (src)
  (let ((lines (mapcar
                #'(lambda (src)
                    (if (string-match "^\\(.*\\)${child}\\(.*\\)$" src)
                        (mapcar (lambda (i)
                                  (match-string i src))
                                '(1 2))
                      (list src)))
                (split-string src "\n"))))
    (labels
        ((iter
          (l m a b)
          (if l
              (if (< 1 (length (car l)))
                  (iter (cdr l)
                        'b
                        (cons (caar l)  a)
                        (cons (cadar l) b))
                (if (eql m 'a)
                    (iter (cdr l) m (cons (caar l) a) b)
                  (iter (cdr l) m a (cons (caar l) b))))
            (if b
                `(lambda (contents)
                   (concat
                    ,(zencoding-join-string (reverse a) "\n")
                    contents
                    ,(zencoding-join-string (reverse b) "\n")))
              `(lambda (contents)
                 (concat
                  ,(zencoding-join-string (reverse a) "\n")
                  contents))))))
      (eval (iter lines 'a nil nil)))))

(defun zencoding-make-html-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
  "Create HTML markup string"
  (zencoding-aif
   (gethash tag-name zencoding-tag-snippets-table)

   (let ((fn (if (stringp it)
                 (zencoding-html-snippets-instantiate-lambda it)
               it)))
     (prog1
         (funcall fn content)
       (puthash tag-name fn zencoding-tag-snippets-table)))

   (let* ((id           (zencoding-concat-or-empty " id=\"" tag-id "\""))
          (classes      (zencoding-mapconcat-or-empty " class=\"" tag-classes " " "\""))
          (props        (let* ((tag-props-default
                                (and settings (gethash "defaultAttr" settings)))
                               (merged-tag-props
                                (zencoding-merge-tag-props
                                 tag-props-default
                                 tag-props)))
                          (zencoding-mapconcat-or-empty
                           " " merged-tag-props " " nil
                           (lambda (prop)
                             (let ((key (car prop)))
                               (concat (if (symbolp key) (symbol-name key) key)
                                       "=\"" (cadr prop) "\""))))))
          (content-multiline? (and content (string-match "\n" content)))
          (block-tag?         (and settings (gethash "block" settings)))
          (self-closing?      (and (not (or tag-txt content))
                                   (or (not tag-has-body?)
                                       (and settings (gethash "selfClosing" settings)))))
          (lf                 (if (or content-multiline? block-tag?) "\n")))
     (concat "<" tag-name id classes props
             (if self-closing? "/>"
               (concat ">"
                       (if tag-txt
                           (if (or content-multiline? block-tag?)
                               (zencoding-indent tag-txt)
                             tag-txt))
                       (if content
                           (if (or content-multiline? block-tag?)
                               (zencoding-indent content)
                             content))
                       lf
                       "</" tag-name ">"))))))

(defun zencoding-make-commented-html-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
  "Create HTML markup string with extra comments for elements with #id or .classes"
  (let ((body (zencoding-make-html-tag tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)))
    (if (or tag-id tag-classes)
        (let ((id      (zencoding-concat-or-empty "#" tag-id))
              (classes (zencoding-mapconcat-or-empty "." tag-classes ".")))
          (concat "<!-- " id classes " -->\n"
                  body
                  "\n<!-- /" id classes " -->"))
      body)))

(defun zencoding-make-haml-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
  "Create HAML string"
  (let ((name    (if (and (equal tag-name "div")
                          (or tag-id tag-classes))
                     ""
                   (concat "%" tag-name)))
        (id      (zencoding-concat-or-empty "#" tag-id))
        (classes (zencoding-mapconcat-or-empty "." tag-classes "."))
        (props   (zencoding-mapconcat-or-empty
                  "{" tag-props ", " "}"
                  (lambda (prop)
                    (concat ":" (symbol-name (car prop)) " => \"" (cadr prop) "\"")))))
    (concat name id classes props
            (if tag-txt
                (zencoding-indent tag-txt))
            (if content
                (zencoding-indent content)))))

(defun zencoding-make-hiccup-tag (tag-name tag-has-body? tag-id tag-classes tag-props tag-txt settings content)
  "Create Hiccup string"
  (let* ((id      (zencoding-concat-or-empty "#" tag-id))
         (classes (zencoding-mapconcat-or-empty "." tag-classes "."))
         (props   (zencoding-mapconcat-or-empty
                   " {" tag-props ", " "}"
                   (lambda (prop)
                     (concat ":" (symbol-name (car prop)) " \"" (cadr prop) "\""))))
         (content-multiline? (and content (string-match "\n" content)))
         (block-tag? (and settings (gethash "block" settings))))
    (concat "[:" tag-name id classes props
            (if tag-txt
                (let ((tag-txt-quoted (concat "\"" tag-txt "\"")))
                  (if (or content-multiline? block-tag?)
                      (zencoding-indent tag-txt-quoted)
                    (concat " " tag-txt-quoted))))
            (if content
                (if (or content-multiline? block-tag?)
                    (zencoding-indent content)
                  (concat " " content)))
            "]")))

(defun zencoding-make-text (tag-maker text)
  (cond
   ((eq tag-maker 'zencoding-make-hiccup-tag)
    (concat "\"" text "\""))
   (t text)))

(defun zencoding-concat-or-empty (prefix body &optional suffix)
  "Return prefixed suffixed text or empty string."
  (if body
      (concat prefix body suffix)
    ""))

(defun zencoding-mapconcat-or-empty (prefix list-body delimiter &optional suffix map-fun)
  "Return prefixed suffixed mapconcated text or empty string."
  (if list-body
      (let* ((mapper (if map-fun map-fun 'identity))
             (body (mapconcat mapper list-body delimiter)))
        (concat prefix body suffix))
    ""))

(defun zencoding-escape-xml (input proc)
  "Escapes XML-unsafe characters: <, > and &."
  (replace-regexp-in-string
   "<" "&lt;"
   (replace-regexp-in-string
    ">" "&gt;"
    (replace-regexp-in-string
     "&" "&amp;"
     (if (stringp input)
         input
       (zencoding-process-filter (zencoding-default-filter) input))))))

(defun zencoding-html-transform (input)
  (let ((ast (car (zencoding-expr input))))
    (when (not (eq ast 'error))
      (zencoding-transform-ast-with-filters ast))))

(defun zencoding-transform-ast-with-filters (ast-with-filters)
  "Transform AST (containing filter data) into string."
  (let ((filters (cadr ast-with-filters))
        (ast (caddr ast-with-filters)))
    (zencoding-process-filter filters ast)))

(defun zencoding-transform-ast (ast tag-maker)
  "Transform AST (without filter data) into string."
  (let ((type (car ast)))
    (cond
     ((eq type 'list)
      (mapconcat (lexical-let ((make-tag-fun tag-maker))
                   #'(lambda (sub-ast)
                       (zencoding-transform-ast sub-ast make-tag-fun)))
                 (cadr ast)
                 "\n"))
     ((eq type 'tag)
      (zencoding-make-tag tag-maker (cadr ast)))
     ((eq type 'text)
      (zencoding-make-text tag-maker (cadr ast)))
     ((eq type 'parent-child)
      (let ((parent (cadadr ast))
            (children (zencoding-transform-ast (caddr ast) tag-maker)))
        (zencoding-make-tag tag-maker parent children)))
     ((eq type 'sibling)
      (let ((sib1 (zencoding-transform-ast (cadr ast) tag-maker))
            (sib2 (zencoding-transform-ast (caddr ast) tag-maker)))
        (concat sib1 "\n" sib2))))))

(defun zencoding-indent (text)
  "Indent the text"
  (if text
      (replace-regexp-in-string "\n" "\n    " (concat "\n" text))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; CSS abbrev:

(zencoding-defparameter
 zencoding-css-unit-aliases
 (gethash "unitAliases" (gethash "css" zencoding-preferences)))
(defun zencoding-css-arg-number (input)
  (zencoding-parse
   " *\\(\\(?:-\\|\\)[0-9.]+\\)\\(\\(?:-\\|e\\|p\\|x\\)\\|\\)" 3 "css number arguments"
   (cons (list (elt it 1)
               (let ((unit (elt it 2)))
                 (gethash unit zencoding-css-unit-aliases "px")))
         input)))

(defun zencoding-css-arg-color (input)
  (zencoding-parse
   " *#\\([0-9a-fA-F]\\{1,6\\}\\)" 2 "css color argument"
   (cons (let* ((n (elt it 1))
                (l (length n)))
           (concat
            "#"
            (substring
             (cond ((= l 1) (concat (make-list 6 (string-to-char n))))
                   ((= l 2) (concat n n n))
                   ((= l 3) (concat
                             (loop for c in (string-to-list n)
                                   append (list c c))))
                   (t (concat n n)))
             0 6)))
         input)))

(defun zencoding-css-arg-something (input)
  (zencoding-parse
   " *\\([^ ]+\\)" 2 "css argument"
   (cons (elt it 1) input)))

(defun zencoding-css-parse-arg (input)
  (zencoding-run zencoding-css-arg-number it
                 (zencoding-run zencoding-css-arg-color it
                                (zencoding-run zencoding-css-arg-something it
                                               (if (equal input "")
                                                   it
                                                 (cons input ""))))))

(defun zencoding-css-important-p (input)
  (let ((len (length input)))
    (and (< 0 len)
         (char-equal (aref input (1- len)) ?!))))

(defun zencoding-css-parse-args (args)
  (when args
    (let ((rt nil))
      (loop
       (zencoding-pif (zencoding-css-parse-arg args)
                      (progn (push (car it) rt)
                             (setf args (cdr it)))
                      (return (nreverse rt)))))))

(defun zencoding-css-split-args (exp)
  (zencoding-aif
   (string-match "\\(?:[ #0-9$]\\|-[0-9]\\)" exp)
   (list (substring exp 0 it) (substring exp it))
   (list exp nil)))

(defun zencoding-css-split-vendor-prefixes (input)
  (zencoding-parse
   "\\(-[wmso]+-\\|-\\|\\)\\(.*\\)" 3 "css vendor prefixes"
   (list (elt it 2)
         (let ((vp (elt it 1)))
           (if (not (string= vp ""))
               (if (string= vp "-") 'auto
                 (string-to-list (subseq vp 1 -1))))))))

(defun zencoding-css-subexpr (exp)
  (let* ((importantp (zencoding-css-important-p exp)))
    (destructuring-bind (exp vp)
        (zencoding-css-split-vendor-prefixes exp)
      (destructuring-bind (key args)
          (zencoding-css-split-args (if importantp (subseq exp 0 -1) exp))
        `(,key ,vp
               ,importantp
               ,@(zencoding-css-parse-args args))))))

(defun zencoding-css-toknize (str)
  (let* ((i (split-string str "+"))
         (rt nil))
    (loop
     (let ((f (first i))
           (s (second i)))
       (if f
           (if (and s (or (string= s "")
                          (string-match "^\\(?:[ #0-9$]\\|-[0-9]\\)" s)))
               (progn
                 (setf rt (cons (concat f "+" s) rt))
                 (setf i (cddr i)))
             (progn
               (setf rt (cons f rt))
               (setf i (cdr i))))
         (return (nreverse rt)))))))

(defun zencoding-css-expr (input)
  (mapcar #'zencoding-css-subexpr
          (zencoding-css-toknize input)))

(zencoding-defparameter
 zencoding-css-snippets
 (gethash "snippets" (gethash "css" zencoding-snippets)))

(zencoding-defparameter
 zencoding-css-unitless-properties
 (gethash "unitlessProperties" (gethash "css" zencoding-preferences)))

(zencoding-defparameter
 zencoding-css-unitless-properties-regex
 (concat "^\\(:?" (zencoding-join-string
                   zencoding-css-unitless-properties "\\|")
         "\\):.*$"))

(defun zencoding-css-instantiate-lambda (str)
  (flet ((insert-space-between-name-and-body
          (str)
          (if (string-match "^\\([a-z-]+:\\)\\(.+\\)$" str)
              (zencoding-join-string
               (mapcar (lambda (ref) (match-string ref str)) '(1 2)) " ")
            str))
         (split-string-to-body
          (str args-sym)
          (let ((rt '(concat)) (idx-max 0))
            (loop for i from 0 to 255 do
                  (zencoding-aif
                   (string-match "\\(?:|\\|${\\(?:\\([0-9]\\):\\|\\)\\(?:\\(.+?\\)\\|\\)}\\)" str)
                   (destructuring-bind (mat idx def)
                       (mapcar (lambda (ref) (match-string ref str)) '(0 1 2))
                     (setf rt
                           `((or
                              (nth ,(let ((cur-idx (if idx (1- (string-to-int idx)) i)))
                                      (setf idx-max (max cur-idx idx-max)))
                                   ,args-sym)
                              ,(or def ""))
                             ,(substring str 0 it) ;; ordered to reverse
                             ,@rt))
                     (setf str (substring str (+ it (length mat)))))
                   ;; don't use nreverse. cause bug in emacs-lisp.
                   (return (cons idx-max (reverse (cons str rt)))))))))
    (let ((args (gensym))
          (str  (insert-space-between-name-and-body str)))
      (destructuring-bind (idx-max . body) (split-string-to-body str args)
        (eval
         `(lambda (&rest ,args)
            (progn
              (when (nthcdr ,idx-max ,args)
                (setf (nthcdr ,idx-max ,args)
                      (list (zencoding-join-string
                             (nthcdr ,idx-max ,args) " "))))
              ,body)))))))

(zencoding-defparameter
 zencoding-vendor-prefixes-properties
 (gethash "vendorPrefixesProperties" (gethash "css" zencoding-preferences)))
(zencoding-defparameter
 zencoding-vendor-prefixes-default
 (list "webkit" "moz" "ms" "o"))
(defun zencoding-css-transform-vendor-prefixes (line vp)
  (let ((key (subseq line 0 (or (position ?: line) (length line)))))
    (let ((vps (if (eql vp 'auto)
                   (gethash key
                            zencoding-vendor-prefixes-properties
                            zencoding-vendor-prefixes-default)
                 (mapcar (lambda (v)
                           (cond ((= v ?w) "webkit")
                                 ((= v ?m) "moz")
                                 ((= v ?s) "ms")
                                 ((= v ?o) "o")))
                         vp))))
      (zencoding-join-string
       (append (mapcar (lambda (v) (concat "-" v "-" line)) vps)
               (list line))
       "\n"))))

(defun zencoding-css-transform-exprs (exprs)
  (zencoding-join-string
   (mapcar
    #'(lambda (expr)
        (let ((basement
               (zencoding-aif
                (gethash (car expr) zencoding-css-snippets)
                (let ((set it) (fn nil) (unitlessp nil))
                  (if (stringp set)
                      (progn
                        ;; new pattern
                        ;; creating print function
                        (setf fn (zencoding-css-instantiate-lambda set))
                        ;; get unitless or no
                        (setf unitlessp
                              (not (null (string-match
                                          zencoding-css-unitless-properties-regex set))))
                        ;; caching
                        (puthash (car expr) (cons fn unitlessp) zencoding-css-snippets))
                    (progn
                      ;; cache hit.
                      (setf fn (car set))
                      (setf unitlessp (cdr set))))
                  (apply fn
                         (mapcar
                          #'(lambda (arg)
                              (if (listp arg)
                                  (if unitlessp (car arg)
                                    (apply #'concat arg))
                                arg))
                          (cdddr expr))))
                (concat (car expr) ": "
                        (zencoding-join-string
                         (mapcar #'(lambda (arg)
                                     (if (listp arg) (apply #'concat arg) arg))
                                 (cdddr expr)) " ")
                        ";"))))
          (let ((line
                 (if (caddr expr)
                     (concat (subseq basement 0 -1) " !important;")
                   basement)))
            (zencoding-aif
             (cadr expr)
             (zencoding-css-transform-vendor-prefixes line it)
             line))))
    exprs)
   "\n"))

(defun zencoding-css-transform (input)
  (zencoding-css-transform-exprs (zencoding-css-expr input)));;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zencoding minor mode

(defgroup zencoding nil
  "Customization group for zencoding-mode."
  :group 'convenience)

(defun zencoding-expr-on-line ()
  "Extract a zencoding expression and the corresponding bounds
   for the current line."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (line (buffer-substring-no-properties start end))
         (expr (zencoding-regex "\\([ \t]*\\)\\([^\n]+\\)" line 2)))
    (if (first expr)
        (list (first expr) start end))))

(defcustom zencoding-indentation 4
  "Number of spaces used for indentation."
  :type '(number :tag "Spaces")
  :group 'zencoding)

(defun zencoding-prettify (markup indent)
  (let ((first-col (format (format "%%%ds" indent) ""))
        (tab       (format (format "%%%ds" zencoding-indentation) "")))
    (concat first-col
            (replace-regexp-in-string "\n" (concat "\n" first-col)
                                      (replace-regexp-in-string "    " tab markup)))))

(defun zencoding-transform (input)
  (if (eql major-mode 'css-mode)
      (zencoding-css-transform input)
    (zencoding-html-transform input)))

;;;###autoload
(defun zencoding-expand-line (arg)
  "Replace the current line's zencode expression with the corresponding expansion.
If prefix ARG is given or region is visible call `zencoding-preview' to start an
interactive preview.

Otherwise expand line directly.

For more information see `zencoding-mode'."
  (interactive "P")
  (let* ((here (point))
         (preview (if zencoding-preview-default (not arg) arg))
         (beg (if preview
                  (progn
                    (beginning-of-line)
                    (skip-chars-forward " \t")
                    (point))
                (when mark-active (region-beginning))))
         (end (if preview
                  (progn
                    (end-of-line)
                    (skip-chars-backward " \t")
                    (point))
                (when mark-active (region-end)))))
    (if beg
        (progn
          (goto-char here)
          (zencoding-preview beg end))
      (let ((expr (zencoding-expr-on-line)))
        (if expr
            (let ((markup (zencoding-transform (first expr))))
              (when markup
                (let ((pretty (zencoding-prettify markup (current-indentation))))
                  (save-excursion
                    (delete-region (second expr) (third expr))
                    (zencoding-insert-and-flash pretty))))))))))

(defvar zencoding-mode-keymap nil
  "Keymap for zencode minor mode.")

(if zencoding-mode-keymap
    nil
  (progn
    (setq zencoding-mode-keymap (make-sparse-keymap))
    (define-key zencoding-mode-keymap (kbd "C-j") 'zencoding-expand-line)
    (define-key zencoding-mode-keymap (kbd "<C-return>") 'zencoding-expand-line)))

;;;###autoload
(define-minor-mode zencoding-mode
  "Minor mode for writing HTML and CSS markup.
With zen coding for HTML and CSS you can write a line like

  ul#name>li.item*2

and have it expanded to

  <ul id=\"name\">
    <li class=\"item\"></li>
    <li class=\"item\"></li>
  </ul>

This minor mode defines keys for quick access:

\\{zencoding-mode-keymap}

Home page URL `http://www.emacswiki.org/emacs/ZenCoding'.

See also `zencoding-expand-line'."
  :lighter " Zen"
  :keymap zencoding-mode-keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zencoding yasnippet integration

(defun zencoding-transform-yas (input)
  (let* ((leaf-count 0)
         (zencoding-leaf-function
          (lambda ()
            (format "$%d" (incf leaf-count)))))
    (zencoding-transform input)))

;;;###autoload
(defun zencoding-expand-yas ()
  (interactive)
  (let ((expr (zencoding-expr-on-line)))
    (if expr
        (let* ((markup (zencoding-transform-yas (first expr)))
               (filled (replace-regexp-in-string "><" ">\n<" markup)))
          (delete-region (second expr) (third expr))
          (insert filled)
          (indent-region (second expr) (point))
          (yas/expand-snippet
           (buffer-substring (second expr) (point))
           (second expr) (point))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Real-time preview
;;

;;;;;;;;;;
;; Lennart's version

(defvar zencoding-preview-input nil)
(make-local-variable 'zencoding-preview-input)
(defvar zencoding-preview-output nil)
(make-local-variable 'zencoding-preview-output)
(defvar zencoding-old-show-paren nil)
(make-local-variable 'zencoding-old-show-paren)

(defface zencoding-preview-input
  '((default :box t :inherit secondary-selection))
  "Face for preview input field."
  :group 'zencoding)

(defface zencoding-preview-output
  '((default :inherit highlight))
  "Face for preview output field."
  :group 'zencoding)

(defvar zencoding-preview-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'zencoding-preview-accept)
    (define-key map (kbd "<return>") 'zencoding-preview-accept)
    (define-key map [(control ?g)] 'zencoding-preview-abort)
    map))

(defun zencoding-preview-accept ()
  (interactive)
  (let ((ovli zencoding-preview-input))
    (if (not (and (overlayp ovli)
                  (bufferp (overlay-buffer ovli))))
        (message "Preview is not active")
      (let* ((indent (current-indentation))
             (markup (zencoding-preview-transformed indent)))
        (when markup
          (delete-region (line-beginning-position) (overlay-end ovli))
          (zencoding-insert-and-flash markup)))))
  (zencoding-preview-abort))

(defvar zencoding-flash-ovl nil)
(make-variable-buffer-local 'zencoding-flash-ovl)

(defun zencoding-remove-flash-ovl (buf)
  (with-current-buffer buf
    (when (overlayp zencoding-flash-ovl)
      (delete-overlay zencoding-flash-ovl))
    (setq zencoding-flash-ovl nil)))

(defcustom zencoding-preview-default t
  "If non-nil then preview is the default action.
This determines how `zencoding-expand-line' works by default."
  :type 'boolean
  :group 'zencoding)

(defcustom zencoding-insert-flash-time 0.5
  "Time to flash insertion.
Set this to a negative number if you do not want flashing the
expansion after insertion."
  :type '(number :tag "Seconds")
  :group 'zencoding)

(defun zencoding-insert-and-flash (markup)
  (zencoding-remove-flash-ovl (current-buffer))
  (let ((here (point)))
    (insert markup)
    (setq zencoding-flash-ovl (make-overlay here (point)))
    (overlay-put zencoding-flash-ovl 'face 'zencoding-preview-output)
    (when (< 0 zencoding-insert-flash-time)
      (run-with-idle-timer zencoding-insert-flash-time
                           nil 'zencoding-remove-flash-ovl (current-buffer)))))

;;;###autoload
(defun zencoding-preview (beg end)
  "Expand zencode between BEG and END interactively.
This will show a preview of the expanded zen code and you can
accept it or skip it."
  (interactive (if mark-active
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (zencoding-preview-abort)
  (if (not beg)
      (message "Region not active")
    (setq zencoding-old-show-paren show-paren-mode)
    (show-paren-mode -1)
    (let ((here (point)))
      (goto-char beg)
      (forward-line 1)
      (unless (= 0 (current-column))
        (insert "\n"))
      (let* ((opos (point))
             (ovli (make-overlay beg end nil nil t))
             (ovlo (make-overlay opos opos))
             (info (propertize " Zen preview. Choose with RET. Cancel by stepping out. \n"
                               'face 'tooltip)))
        (overlay-put ovli 'face 'zencoding-preview-input)
        (overlay-put ovli 'keymap zencoding-preview-keymap)
        (overlay-put ovlo 'face 'zencoding-preview-output)
        (overlay-put ovlo 'before-string info)
        (setq zencoding-preview-input  ovli)
        (setq zencoding-preview-output ovlo)
        (add-hook 'before-change-functions 'zencoding-preview-before-change t t)
        (goto-char here)
        (add-hook 'post-command-hook 'zencoding-preview-post-command t t)))))

(defvar zencoding-preview-pending-abort nil)
(make-variable-buffer-local 'zencoding-preview-pending-abort)

(defun zencoding-preview-before-change (beg end)
  (when
      (or (> beg (overlay-end zencoding-preview-input))
          (< beg (overlay-start zencoding-preview-input))
          (> end (overlay-end zencoding-preview-input))
          (< end (overlay-start zencoding-preview-input)))
    (setq zencoding-preview-pending-abort t)))

(defun zencoding-preview-abort ()
  "Abort zen code preview."
  (interactive)
  (setq zencoding-preview-pending-abort nil)
  (remove-hook 'before-change-functions 'zencoding-preview-before-change t)
  (when (overlayp zencoding-preview-input)
    (delete-overlay zencoding-preview-input))
  (setq zencoding-preview-input nil)
  (when (overlayp zencoding-preview-output)
    (delete-overlay zencoding-preview-output))
  (setq zencoding-preview-output nil)
  (remove-hook 'post-command-hook 'zencoding-preview-post-command t)
  (when zencoding-old-show-paren (show-paren-mode 1)))

(defun zencoding-preview-post-command ()
  (condition-case err
      (zencoding-preview-post-command-1)
    (error (message "zencoding-preview-post: %s" err))))

(defun zencoding-preview-post-command-1 ()
  (if (and (not zencoding-preview-pending-abort)
           (<= (point) (overlay-end zencoding-preview-input))
           (>= (point) (overlay-start zencoding-preview-input)))
      (zencoding-update-preview (current-indentation))
    (zencoding-preview-abort)))

(defun zencoding-preview-transformed (indent)
  (let* ((string (buffer-substring-no-properties
		  (overlay-start zencoding-preview-input)
		  (overlay-end zencoding-preview-input))))
    (let ((output (zencoding-transform string)))
      (when output
        (zencoding-prettify output indent)))))

(defun zencoding-update-preview (indent)
  (let* ((pretty (zencoding-preview-transformed indent))
         (show (when pretty
                 (propertize pretty 'face 'highlight))))
    (when show
      (overlay-put zencoding-preview-output 'after-string
                   (concat show "\n")))))

(provide 'zencoding-mode)

;;; zencoding-mode.el ends here