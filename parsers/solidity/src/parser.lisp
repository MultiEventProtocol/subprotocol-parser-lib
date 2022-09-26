;; Copyright © 2021-2022 Glukhov Mikhail. All rights reserved. Licensed
;; under the GNU AGPLv3
(in-package #:solipsism)

(define-parser *sol-parser*
  (:start-symbol %src-unit)
  (:terminals (
               %import %as
               %constant
               %pragma
               %library
               %using
               %event %anonymous %indexed
               %enum
               %contract %abstract %identifier %is |%.| |%,| |%{| |%}|
               ;; |%:=|
               |%:| |%(| |%)|
               %function |%;|
               %data-location
               %returns
               %visibility
               %pure
               %view
               %payable
               %virtual
               %override
               |%=>| %mapping
               %unsigned-integer-type %signed-integer-type %fixed-bytes %string
               %bytes %fixed %ufixed %bool %address %payable
               %unchecked
               %constructor
               %interface
               %modifier
               %fallback
               %receive
               %struct
               %error
               %immutable
               %if %else
               %for
               %while
               %do
               %continue
               %break
               %try
               %catch
               %return
               %emit
               %revert
               %assembly %evmasm
               %leave
               %let
               ;; %yul-identifier
               ;; %asm-command
               ;; %yul-dec-lit
               ;; %yul-bool-lit
               ;; %yul-hex-num
               %switch %case %default
               |%->|
               |%[| |%]| |.| %addr %type |%++| |%--| |%!| |%~| %delete |%-| |%*| |%**| |%/| |%%| |%+| |%-| |%>>>| |%>>| |%<<| |%&| |%^| |%pipe| |%<| |%>| |%<=| |%>=| |%==| |%=| |%!=| |%&&| |%pipepipe| |%?| |%pipe=| |%^=| |%&=| |%<<=| |%>>=| |%>>>=| |%+=| |%-=| |%*=| |%/=| |%%=| %new
               ;; %built-in-func
               %single-quoted-printable-empty %double-quoted-printable-empty
               %single-quoted-printable %double-quoted-printable
               %dec_num_v1
               %hex_num_v1
               %hex_num_v2
               %hex_num_v3
               %bool-lit
               ))

  (%src-unit
   (%src-unit-contents #'(lambda (last) `(:src (,last))))
   (%src-unit-contents %src-unit
                       #'(lambda (head rest)
                           `(:src ,(append (list head) (cadr rest)))))
   )
  (%src-unit-contents
   (%pragma-def #'(lambda (df) `(:pragma-def ,df)))
   (%import-def #'(lambda (df) `(:import-def ,df)))
   (%ctract-def #'(lambda (df) `(:ctract-def ,df)))
   (%iface-def  #'(lambda (df) `(:iface-def ,df)))
   (%lib-def    #'(lambda (df) `(:lib-def ,df)))
   (%func-def   #'(lambda (df) `(:func-def ,df)))
   (%const-def #'(lambda (cf) `(:const-def ,cf)))
   (%struct-def #'(lambda (x) `(:struct-def ,x)))
   (%enum-def #'(lambda (df) `(:enum-def ,df)))
   (%user-val-type-def #'(lambda (df) `(:user-val-type-def ,df)))
   (%error-def #'(lambda (df) `(:error-def ,df)))
   )
  (%import-def
   (%import %non-empty-str %ident |%;|
            #'(lambda (im str id sc) `(:str-lit ,str :ident-allias ,id)))
   (%import %non-empty-str |%;|
            #'(lambda (im str sc) `(:str-lit ,str)))
   (%import %symbol-alliases %from %non-empty-str |%;|
            #'(lambda (im allias fr str sc)
                `(:allias ,allias :str-lit ,str)))
  
   )
  
  (%symbol-alliases
   ( |%{| %symbol-alliases-body |}%| #'(lambda (lb df rb) `(:error-def ,df))))
  
  (%symbol-alliases-body
   (%ident #'(lambda (head) `(:ident-allias ,head)))
   (%ident |%,| %symbol-alliases-body
           #'(lambda (head comma rest)
               `(:ident-allias ,(append (list head) (cadr rest)))))
   (%ident %as %ident #'(lambda (id1 as id2)
                           `(:ident1-allias ,id1 :ident2-allias ,id2)))
   (%ident %as %ident |%,| %symbol-alliases-body
            #'(lambda (id1 as id2 comma symb-rest)
                `(:ident1-allias ,id1 :ident2-allias ,id2 :sybl-body-rest ,symb-rest)))
   )
  
  
  (%const-def
   (%type-name %constant %identifier |%=| %expr |%;|
               #'(lambda (t-name const id eq ex end)
                   `(:c-t-name ,t-name :c-id ,id :c-expr ,ex)))
  
   )
  (%pragma-def
   (%pragma #'(lambda (pr) `(:pragma ,pr)))
   )
  (%lib-def
   (%library %identifier |%{| |%}|
             #'(lambda (library id l-brak r-brak)
                 `(:library ,id :contents :empty)))
   (%library %identifier |%{| %ctract-body-elt-contents |%}|
             #'(lambda (library id l-brak bdy r-brak)
                 `(:library ,id :contents ,bdy)))
   )
  (%ctract-body-elt-contents
   (%ctract-body-elt #'(lambda (last) `(:ctract-body-elt (,last))))
   (%ctract-body-elt %ctract-body-elt-contents
                     #'(lambda (head rest)
                         `(:ctract-body-elt ,(append (list head) (cadr rest)))))
   )
  (%ctract-body-elt
   (%func-def #'(lambda (x) `(:func-def ,x)))
   (%state-var-decl #'(lambda (x) `(:state-var-decl ,x)))
   (%ctor-def #'(lambda (x) `(:ctor ,x)))
   (%modif-def #'(lambda (x) `(:modif-def ,x)))
   (%fallback-func-def #'(lambda (x) `(:fallback-func-def ,x)))
   (%enum-def #'(lambda (x) `(:enum-def ,x)))
   (%receive-func-def #'(lambda (x) `(:receive-func-def ,x)))
   (%struct-def #'(lambda (x) `(:struct-def ,x)))
   (%user-val-type-def #'(lambda (df) `(:user-val-type-def ,df)))
   (%event-def #'(lambda (x) `(:event-def ,x)))
   (%error-def #'(lambda (df) `(:error-def ,df)))
   (%using-def #'(lambda (uf) `(:using-def ,uf)))
   )
  (%using-def
   (%using %ident-path %for |%*| |%;|
           #'(lambda (us id fo st sc)
               `(:using ,id :all t)))
   (%using %ident-path %for %type-name |%;|
           #'(lambda (us id fo tn sc)
               `(:using ,id :type-name ,tn))))
  (%event-def
   (%event %identifier |%(| |%)| |%;|
           #'(lambda (ev id lb rb sc) `(:event ,id)))
   (%event %identifier |%(| |%)| %anonymous |%;|
           #'(lambda (ev id lb rb an sc)`(:event ,id :anon t)))
   (%event %identifier |%(| %event-params-contents |%)| |%;|
           #'(lambda (ev id lb ep rb sc) `(:event ,id :params ,ep)))
   (%event %identifier |%(| %event-params-contents |%)| %anonymous |%;|
           #'(lambda (ev id lb ep rb an sc)
               `(:event ,id :params ,ep :anon t)))
   )
  (%event-params-contents
   (%event-param #'(lambda (last) `(:event-param (,last))))
   (%event-param |%,| %event-params-contents
                 #'(lambda (head co rest)
                     `(:event-param ,(append (list head) (cadr rest)))))
   )
  (%event-param
   (%type-name #'(lambda (tn) `(:type-name ,tn)))
   (%type-name %indexed
               #'(lambda (tn ix) `(:type-name ,tn :indexed ,ix)))
   (%type-name %identifier
               #'(lambda (tn id) `(:type-name ,tn :id ,id)))
   (%type-name %indexed %identifier
               #'(lambda (tn ix id) `(:type-name ,tn :indexed ,ix :id ,id)))
   )
  (%enum-def
   (%enum %identifier |%{| %multi-ident-path |%}|
          #'(lambda (en id lb co rb)
              `(:enum ,id :contents, co)))
   )
  (%ctract-def
   ;; empty contract
   (%contract %identifier |%{| |%}|
              #'(lambda (ctract id l-brak r-brak) `(:contract ,id)))
   ;; with contract-body
   (%contract %identifier |%{| %ctract-body-elt-contents |%}|
              #'(lambda (ctract id l-brak cbec r-brak)
                  `(:contract ,id :contents ,cbec)))
   ;; abstract empty contract
   (%abstract %contract %identifier |%{| |%}|
              #'(lambda (ab ctract id l-brak r-brak) `(:contract ,id :abstract t)))
   ;; abstract with contract-body
   (%abstract %contract %identifier |%{| %ctract-body-elt-contents |%}|
              #'(lambda (ab ctract id l-brak cbec r-brak)
                  `(:contract ,id :abstract t :contents ,cbec)))
   ;; INCHERITANCE
   ;; empty inheritance contract
   (%contract %identifier %is %inher-spec-contents |%{| |%}|
              #'(lambda (ctract id is isl l-brak r-brak)
                  `(:contract ,id :inher ,isl)))
   ;; inheritance with contract-body
   (%contract %identifier %is %inher-spec-contents |%{| %ctract-body-elt-contents |%}|
              #'(lambda (ctract id is isl l-brak cbec r-brak)
                  `(:contract ,id :inher ,isl :contents ,cbec)))
   ;; abstract empty inheritance contract
   (%abstract %contract %identifier %is %inher-spec-contents |%{| |%}|
              #'(lambda (ab ctract id is isl l-brak r-brak)
                  `(:contract ,id :abstract t :inher ,isl)))
   ;; abstract inheritance with contract-body
   (%abstract %contract %identifier %is %inher-spec-contents |%{| %ctract-body-elt-contents |%}|
              #'(lambda (ab ctract id is isl l-brak cbec r-brak)
                  `(:contract ,id :abstract t :inher ,isl :contents ,cbec)))
   )
  (%inher-spec-contents
   (%inher-spec #'(lambda (last) `(:inher-spec (,last))))
   (%inher-spec |%,| %inher-spec-contents
                #'(lambda (head co rest)
                    `(:inher-spec ,(append (list head) (cadr rest)))))
   )
  (%inher-spec
   (%ident-path #'(lambda (ip) `(:ident-path ,ip)))
   ;; (%ident-path %call-arg-list ;; NB: call-arg-list
   ;;              #'(lambda (ip al) `(:identifier-path ,ip :call-arg-list ,al)))
   )
  (%call-arg-list
   (|%(| |%)| #'(lambda (lb rb) `(:call-arg-list nil)))
   (|%(| %identifier %non-empty-str |%)|
         #'(lambda (lb ident str rb) `(:call-arg-id ,ident :call-arg-str ,str )))
   (|%(| %expr-comma-list |%)| #'(lambda (lb ex rb) `(:call-arg-list ,ex)))
   (|%(| |%{|  |%}| |%)|
         #'(lambda (lb lc rc rb) `(:call-arg-list-empty nil)))
   (|%(| |%{| %call-arg-obj |%}| |%)|
         #'(lambda (lb lc ob rc rb) `(:call-arg-list ,ob)))
   )
  (%call-arg-obj
   (%identifier |%:| %expr #'(lambda (id cm ex)
                               `(:call-arg-obj-last ,id :call-expr ,ex)))
   (%identifier |%:| %expr |%,| %call-arg-obj
                #'(lambda (id cm ex sc rt)
                    `(:call-arg-obj-head ,id :call-expr ,ex
                      :call-arg-obj-rest ,rt)))
   )
  (%expr-comma-list
   (%expr #'(lambda (last) `(:expr-comma-list (,last))))
   (%expr |%,| %expr-comma-list
          #'(lambda (head cm rest)
              `(:expr-comma-list ,(append (list head) (cadr rest)))))
   )
  (%func-def
   (%function %identifier %parlist %block
              #'(lambda (fun id parlist blk)
                  `(:fun ,id :parlist ,parlist :block ,blk)))
   (%function %identifier %parlist %retlist %block
              #'(lambda (fn id pl rl blk)
                  `(:fun ,id :parlist ,pl :retlist ,rl :block ,blk)))
   
   (%function %identifier %parlist %fmeta %retlist %block
              #'(lambda (fun id parlist fmeta retlist blk)
                  `(:fun ,id :parlist ,parlist :fmeta ,fmeta :retlist ,retlist
                    :block ,blk)))
   
   (%function %identifier %parlist %fmeta %block
              #'(lambda (fun id parlist fmeta blk)
                  `(:fun ,id :parlist ,parlist :fmeta ,fmeta :block ,blk)))
   
   )
  (%parlist
   (|%(| |%)| #'(lambda (lb rb) `(:param-list nil)))
   (|%(| %param-list |%)| #'(lambda (lb pl rb) `(:param-list ,pl)))
   )
  (%param-list
   (%param #'(lambda (last) `(:par (,last))))
   (%param |%,| %param-list #'(lambda (head cm rest)
                                `(:par ,(append (list head) (cadr rest)))))
   )
  (%param
   (%type-name #'(lambda (x) `(:par-type ,x)))
   (%type-name %error
                #'(lambda (id err) `(:par-id ,id)))
   (%type-name %identifier
               #'(lambda (pt nm) `(:par-type ,pt :name ,nm)))
   (%type-name %data-location
               #'(lambda (pt dl) `(:par-type ,pt :data-location ,dl)))
   (%type-name %data-location %identifier
               #'(lambda (tn dl id) `(:par-type ,tn :data-location ,dl :name ,id)))
   )
  (%retlist
   (%returns |%(| |%)| #'(lambda (rt lb rb) `(:retlist nil)))
   (%returns |%(| %param-list |%)|
             #'(lambda (rt lb rl rb) `(:retlist ,rl)))
   )
  (%fmeta
   (%visibility %override-specifier %state-mutability %virtual %modifier-invocation
     #'(lambda (vis ove sta vir mod)
         `( :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod )))
   (%visibility %override-specifier %state-mutability %modifier-invocation %virtual
     #'(lambda (vis ove sta mod vir)
         `( :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir )))
   (%visibility %override-specifier %modifier-invocation %virtual %state-mutability
     #'(lambda (vis ove mod vir sta)
         `( :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta )))
   (%visibility %override-specifier %modifier-invocation %state-mutability %virtual
     #'(lambda (vis ove mod sta vir)
         `( :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir )))
   (%visibility %override-specifier %virtual %modifier-invocation %state-mutability
     #'(lambda (vis ove vir mod sta)
         `( :visibility ,vis  :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%visibility %override-specifier %virtual %state-mutability %modifier-invocation
     #'(lambda (vis ove vir sta mod)
         `( :visibility ,vis  :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%visibility %virtual %state-mutability %override-specifier %modifier-invocation
     #'(lambda (vis vir sta ove mod)
         `( :visibility ,vis  :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%visibility %virtual %state-mutability %modifier-invocation %override-specifier
     #'(lambda (vis vir sta mod ove)
         `( :visibility ,vis  :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%visibility %virtual %modifier-invocation %override-specifier %state-mutability
     #'(lambda (vis vir mod ove sta)
         `( :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta )))
   (%visibility %virtual %modifier-invocation %state-mutability %override-specifier
     #'(lambda (vis vir mod sta ove)
         `( :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove )))
   (%visibility %virtual %override-specifier %modifier-invocation %state-mutability
     #'(lambda (vis vir ove mod sta)
         `( :visibility ,vis  :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%visibility %virtual %override-specifier %state-mutability %modifier-invocation
     #'(lambda (vis vir ove sta mod)
         `( :visibility ,vis  :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%visibility %modifier-invocation %state-mutability %override-specifier %virtual
     #'(lambda (vis mod sta ove vir)
         `( :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir )))
   (%visibility %modifier-invocation %state-mutability %virtual %override-specifier
     #'(lambda (vis mod sta vir ove)
         `( :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove )))
   (%visibility %modifier-invocation %virtual %override-specifier %state-mutability
     #'(lambda (vis mod vir ove sta)
         `( :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta )))
   (%visibility %modifier-invocation %virtual %state-mutability %override-specifier
     #'(lambda (vis mod vir sta ove)
         `( :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove )))
   (%visibility %modifier-invocation %override-specifier %virtual %state-mutability
     #'(lambda (vis mod ove vir sta)
         `( :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta )))
   (%visibility %modifier-invocation %override-specifier %state-mutability %virtual
     #'(lambda (vis mod ove sta vir)
         `( :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir )))
   (%visibility %state-mutability %modifier-invocation %override-specifier %virtual
     #'(lambda (vis sta mod ove vir)
         `( :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir )))
   (%visibility %state-mutability %modifier-invocation %virtual %override-specifier
     #'(lambda (vis sta mod vir ove)
         `( :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove )))
   (%visibility %state-mutability %virtual %override-specifier %modifier-invocation
     #'(lambda (vis sta vir ove mod)
         `( :visibility ,vis  :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%visibility %state-mutability %virtual %modifier-invocation %override-specifier
     #'(lambda (vis sta vir mod ove)
         `( :visibility ,vis  :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%visibility %state-mutability %override-specifier %virtual %modifier-invocation
     #'(lambda (vis sta ove vir mod)
         `( :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod )))
   (%visibility %state-mutability %override-specifier %modifier-invocation %virtual
     #'(lambda (vis sta ove mod vir)
         `( :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir )))
   (%state-mutability %override-specifier %visibility %virtual %modifier-invocation
     #'(lambda (sta ove vis vir mod)
         `( :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod )))
   (%state-mutability %override-specifier %visibility %modifier-invocation %virtual
     #'(lambda (sta ove vis mod vir)
         `( :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir )))
   (%state-mutability %override-specifier %modifier-invocation %virtual %visibility
     #'(lambda (sta ove mod vir vis)
         `( :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis )))
   (%state-mutability %override-specifier %modifier-invocation %visibility %virtual
     #'(lambda (sta ove mod vis vir)
         `( :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir )))
   (%state-mutability %override-specifier %virtual %modifier-invocation %visibility
     #'(lambda (sta ove vir mod vis)
         `( :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis )))
   (%state-mutability %override-specifier %virtual %visibility %modifier-invocation
     #'(lambda (sta ove vir vis mod)
         `( :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod )))
   (%state-mutability %virtual %visibility %override-specifier %modifier-invocation
     #'(lambda (sta vir vis ove mod)
         `( :state-mutability ,sta  :virtual ,vir  :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%state-mutability %virtual %visibility %modifier-invocation %override-specifier
     #'(lambda (sta vir vis mod ove)
         `( :state-mutability ,sta  :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%state-mutability %virtual %modifier-invocation %override-specifier %visibility
     #'(lambda (sta vir mod ove vis)
         `( :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis )))
   (%state-mutability %virtual %modifier-invocation %visibility %override-specifier
     #'(lambda (sta vir mod vis ove)
         `( :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove )))
   (%state-mutability %virtual %override-specifier %modifier-invocation %visibility
     #'(lambda (sta vir ove mod vis)
         `( :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis )))
   (%state-mutability %virtual %override-specifier %visibility %modifier-invocation
     #'(lambda (sta vir ove vis mod)
         `( :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod )))
   (%state-mutability %modifier-invocation %visibility %override-specifier %virtual
     #'(lambda (sta mod vis ove vir)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove  :virtual ,vir )))
   (%state-mutability %modifier-invocation %visibility %virtual %override-specifier
     #'(lambda (sta mod vis vir ove)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir  :override-specifier ,ove )))
   (%state-mutability %modifier-invocation %virtual %override-specifier %visibility
     #'(lambda (sta mod vir ove vis)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove  :visibility ,vis )))
   (%state-mutability %modifier-invocation %virtual %visibility %override-specifier
     #'(lambda (sta mod vir vis ove)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis  :override-specifier ,ove )))
   (%state-mutability %modifier-invocation %override-specifier %virtual %visibility
     #'(lambda (sta mod ove vir vis)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir  :visibility ,vis )))
   (%state-mutability %modifier-invocation %override-specifier %visibility %virtual
     #'(lambda (sta mod ove vis vir)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis  :virtual ,vir )))
   (%state-mutability %modifier-invocation %override-specifier %virtual
     #'(lambda (sta mod ove vir)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir )))
   (%state-mutability %visibility %modifier-invocation %override-specifier %virtual
     #'(lambda (sta vis mod ove vir)
         `( :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir )))
   (%visibility %modifier-invocation %override-specifier %virtual
     #'(lambda (vis mod ove vir)
         `( :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir )))
   (%state-mutability %modifier-invocation %virtual %override-specifier
     #'(lambda (sta mod vir ove)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove )))
   (%state-mutability %visibility %modifier-invocation %virtual %override-specifier
     #'(lambda (sta vis mod vir ove)
         `( :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove )))
   (%visibility %modifier-invocation %virtual %override-specifier
     #'(lambda (vis mod vir ove)
         `( :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove )))
   (%state-mutability %virtual %override-specifier %modifier-invocation
     #'(lambda (sta vir ove mod)
         `( :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%state-mutability %visibility %virtual %override-specifier %modifier-invocation
     #'(lambda (sta vis vir ove mod)
         `( :state-mutability ,sta  :visibility ,vis  :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%visibility %virtual %override-specifier %modifier-invocation
     #'(lambda (vis vir ove mod)
         `( :visibility ,vis  :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%state-mutability %virtual %modifier-invocation %override-specifier
     #'(lambda (sta vir mod ove)
         `( :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%state-mutability %visibility %virtual %modifier-invocation %override-specifier
     #'(lambda (sta vis vir mod ove)
         `( :state-mutability ,sta  :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%visibility %virtual %modifier-invocation %override-specifier
     #'(lambda (vis vir mod ove)
         `( :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%state-mutability %override-specifier %virtual %modifier-invocation
     #'(lambda (sta ove vir mod)
         `( :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod )))
   (%state-mutability %visibility %override-specifier %virtual %modifier-invocation
     #'(lambda (sta vis ove vir mod)
         `( :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod )))
   (%visibility %override-specifier %virtual %modifier-invocation
     #'(lambda (vis ove vir mod)
         `( :visibility ,vis  :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod )))
   (%state-mutability %override-specifier %modifier-invocation %virtual
     #'(lambda (sta ove mod vir)
         `( :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir )))
   (%state-mutability %visibility %override-specifier %modifier-invocation %virtual
     #'(lambda (sta vis ove mod vir)
         `( :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir )))
   (%visibility %override-specifier %modifier-invocation %virtual
     #'(lambda (vis ove mod vir)
         `( :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir )))
   (%modifier-invocation %override-specifier %visibility %virtual %state-mutability
     #'(lambda (mod ove vis vir sta)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis  :virtual ,vir  :state-mutability ,sta )))
   (%modifier-invocation %override-specifier %visibility %state-mutability %virtual
     #'(lambda (mod ove vis sta vir)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta  :virtual ,vir )))
   (%modifier-invocation %override-specifier %state-mutability %virtual %visibility
     #'(lambda (mod ove sta vir vis)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir  :visibility ,vis )))
   (%modifier-invocation %override-specifier %state-mutability %visibility %virtual
     #'(lambda (mod ove sta vis vir)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis  :virtual ,vir )))
   (%modifier-invocation %override-specifier %virtual %state-mutability %visibility
     #'(lambda (mod ove vir sta vis)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta  :visibility ,vis )))
   (%modifier-invocation %override-specifier %virtual %visibility %state-mutability
     #'(lambda (mod ove vir vis sta)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir  :visibility ,vis  :state-mutability ,sta )))
   (%modifier-invocation %virtual %visibility %override-specifier %state-mutability
     #'(lambda (mod vir vis ove sta)
         `( :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta )))
   (%modifier-invocation %virtual %visibility %state-mutability %override-specifier
     #'(lambda (mod vir vis sta ove)
         `( :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove )))
   (%modifier-invocation %virtual %state-mutability %override-specifier %visibility
     #'(lambda (mod vir sta ove vis)
         `( :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis )))
   (%modifier-invocation %virtual %state-mutability %visibility %override-specifier
     #'(lambda (mod vir sta vis ove)
         `( :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove )))
   (%modifier-invocation %virtual %override-specifier %state-mutability %visibility
     #'(lambda (mod vir ove sta vis)
         `( :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis )))
   (%modifier-invocation %virtual %override-specifier %visibility %state-mutability
     #'(lambda (mod vir ove vis sta)
         `( :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta )))
   (%modifier-invocation %state-mutability %visibility %override-specifier %virtual
     #'(lambda (mod sta vis ove vir)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove  :virtual ,vir )))
   (%state-mutability %visibility %override-specifier %virtual
     #'(lambda (sta vis ove vir)
         `( :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove  :virtual ,vir )))
   (%modifier-invocation %state-mutability %visibility %virtual %override-specifier
     #'(lambda (mod sta vis vir ove)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis  :virtual ,vir  :override-specifier ,ove )))
   (%state-mutability %visibility %virtual %override-specifier
     #'(lambda (sta vis vir ove)
         `( :state-mutability ,sta  :visibility ,vis  :virtual ,vir  :override-specifier ,ove )))
   (%modifier-invocation %virtual %override-specifier %visibility
     #'(lambda (mod vir ove vis)
         `( :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove  :visibility ,vis )))
   (%modifier-invocation %state-mutability %virtual %override-specifier %visibility
     #'(lambda (mod sta vir ove vis)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove  :visibility ,vis )))
   (%state-mutability %virtual %override-specifier %visibility
     #'(lambda (sta vir ove vis)
         `( :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove  :visibility ,vis )))
   (%modifier-invocation %virtual %visibility %override-specifier
     #'(lambda (mod vir vis ove)
         `( :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis  :override-specifier ,ove )))
   (%modifier-invocation %state-mutability %virtual %visibility %override-specifier
     #'(lambda (mod sta vir vis ove)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir  :visibility ,vis  :override-specifier ,ove )))
   (%state-mutability %virtual %visibility %override-specifier
     #'(lambda (sta vir vis ove)
         `( :state-mutability ,sta  :virtual ,vir  :visibility ,vis  :override-specifier ,ove )))
   (%modifier-invocation %override-specifier %virtual %visibility
     #'(lambda (mod ove vir vis)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir  :visibility ,vis )))
   (%modifier-invocation %state-mutability %override-specifier %virtual %visibility
     #'(lambda (mod sta ove vir vis)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir  :visibility ,vis )))
   (%state-mutability %override-specifier %virtual %visibility
     #'(lambda (sta ove vir vis)
         `( :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir  :visibility ,vis )))
   (%modifier-invocation %override-specifier %visibility %virtual
     #'(lambda (mod ove vis vir)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis  :virtual ,vir )))
   (%modifier-invocation %state-mutability %override-specifier %visibility %virtual
     #'(lambda (mod sta ove vis vir)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis  :virtual ,vir )))
   (%state-mutability %override-specifier %visibility %virtual
     #'(lambda (sta ove vis vir)
         `( :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis  :virtual ,vir )))
   (%modifier-invocation %state-mutability %override-specifier %virtual
     #'(lambda (mod sta ove vir)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir )))
   (%state-mutability %override-specifier %virtual
     #'(lambda (sta ove vir)
         `( :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir )))
   (%modifier-invocation %visibility %state-mutability %override-specifier %virtual
     #'(lambda (mod vis sta ove vir)
         `( :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir )))
   (%visibility %state-mutability %override-specifier %virtual
     #'(lambda (vis sta ove vir)
         `( :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove  :virtual ,vir )))
   (%modifier-invocation %state-mutability %virtual %override-specifier
     #'(lambda (mod sta vir ove)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove )))
   (%state-mutability %virtual %override-specifier
     #'(lambda (sta vir ove)
         `( :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove )))
   (%modifier-invocation %visibility %state-mutability %virtual %override-specifier
     #'(lambda (mod vis sta vir ove)
         `( :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove )))
   (%visibility %state-mutability %virtual %override-specifier
     #'(lambda (vis sta vir ove)
         `( :visibility ,vis  :state-mutability ,sta  :virtual ,vir  :override-specifier ,ove )))
   (%modifier-invocation %virtual %override-specifier %state-mutability
     #'(lambda (mod vir ove sta)
         `( :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta )))
   (%modifier-invocation %visibility %virtual %override-specifier %state-mutability
     #'(lambda (mod vis vir ove sta)
         `( :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta )))
   (%visibility %virtual %override-specifier %state-mutability
     #'(lambda (vis vir ove sta)
         `( :visibility ,vis  :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta )))
   (%modifier-invocation %virtual %override-specifier
     #'(lambda (mod vir ove)
         `( :modifier-invocation ,mod  :virtual ,vir  :override-specifier ,ove )))
   (%modifier-invocation %visibility %virtual %override-specifier
     #'(lambda (mod vis vir ove)
         `( :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir  :override-specifier ,ove )))
   (%visibility %virtual %override-specifier
     #'(lambda (vis vir ove)
         `( :visibility ,vis  :virtual ,vir  :override-specifier ,ove )))
   (%modifier-invocation %virtual %state-mutability %override-specifier
     #'(lambda (mod vir sta ove)
         `( :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove )))
   (%modifier-invocation %visibility %virtual %state-mutability %override-specifier
     #'(lambda (mod vis vir sta ove)
         `( :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove )))
   (%visibility %virtual %state-mutability %override-specifier
     #'(lambda (vis vir sta ove)
         `( :visibility ,vis  :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove )))
   (%modifier-invocation %override-specifier %virtual %state-mutability
     #'(lambda (mod ove vir sta)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta )))
   (%modifier-invocation %visibility %override-specifier %virtual %state-mutability
     #'(lambda (mod vis ove vir sta)
         `( :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta )))
   (%visibility %override-specifier %virtual %state-mutability
     #'(lambda (vis ove vir sta)
         `( :visibility ,vis  :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta )))
   (%modifier-invocation %override-specifier %virtual
     #'(lambda (mod ove vir)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :virtual ,vir )))
   (%modifier-invocation %visibility %override-specifier %virtual
     #'(lambda (mod vis ove vir)
         `( :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove  :virtual ,vir )))
   (%visibility %override-specifier %virtual
     #'(lambda (vis ove vir)
         `( :visibility ,vis  :override-specifier ,ove  :virtual ,vir )))
   (%modifier-invocation %override-specifier %state-mutability %virtual
     #'(lambda (mod ove sta vir)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir )))
   (%modifier-invocation %visibility %override-specifier %state-mutability %virtual
     #'(lambda (mod vis ove sta vir)
         `( :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir )))
   (%visibility %override-specifier %state-mutability %virtual
     #'(lambda (vis ove sta vir)
         `( :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir )))
   (%virtual %override-specifier %visibility %modifier-invocation %state-mutability
     #'(lambda (vir ove vis mod sta)
         `( :virtual ,vir  :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%virtual %override-specifier %visibility %state-mutability %modifier-invocation
     #'(lambda (vir ove vis sta mod)
         `( :virtual ,vir  :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%virtual %override-specifier %state-mutability %modifier-invocation %visibility
     #'(lambda (vir ove sta mod vis)
         `( :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis )))
   (%virtual %override-specifier %state-mutability %visibility %modifier-invocation
     #'(lambda (vir ove sta vis mod)
         `( :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod )))
   (%virtual %override-specifier %modifier-invocation %state-mutability %visibility
     #'(lambda (vir ove mod sta vis)
         `( :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis )))
   (%virtual %override-specifier %modifier-invocation %visibility %state-mutability
     #'(lambda (vir ove mod vis sta)
         `( :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta )))
   (%virtual %modifier-invocation %visibility %override-specifier %state-mutability
     #'(lambda (vir mod vis ove sta)
         `( :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta )))
   (%modifier-invocation %visibility %override-specifier %state-mutability
     #'(lambda (mod vis ove sta)
         `( :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta )))
   (%virtual %modifier-invocation %visibility %state-mutability %override-specifier
     #'(lambda (vir mod vis sta ove)
         `( :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove )))
   (%modifier-invocation %visibility %state-mutability %override-specifier
     #'(lambda (mod vis sta ove)
         `( :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove )))
   (%virtual %modifier-invocation %state-mutability %override-specifier %visibility
     #'(lambda (vir mod sta ove vis)
         `( :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis )))
   (%modifier-invocation %state-mutability %override-specifier %visibility
     #'(lambda (mod sta ove vis)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis )))
   (%virtual %modifier-invocation %state-mutability %visibility %override-specifier
     #'(lambda (vir mod sta vis ove)
         `( :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove )))
   (%modifier-invocation %state-mutability %visibility %override-specifier
     #'(lambda (mod sta vis ove)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove )))
   (%virtual %override-specifier %state-mutability %visibility
     #'(lambda (vir ove sta vis)
         `( :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis )))
   (%virtual %modifier-invocation %override-specifier %state-mutability %visibility
     #'(lambda (vir mod ove sta vis)
         `( :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis )))
   (%modifier-invocation %override-specifier %state-mutability %visibility
     #'(lambda (mod ove sta vis)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis )))
   (%virtual %override-specifier %visibility %state-mutability
     #'(lambda (vir ove vis sta)
         `( :virtual ,vir  :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta )))
   (%virtual %modifier-invocation %override-specifier %visibility %state-mutability
     #'(lambda (vir mod ove vis sta)
         `( :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta )))
   (%modifier-invocation %override-specifier %visibility %state-mutability
     #'(lambda (mod ove vis sta)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta )))
   (%virtual %state-mutability %visibility %override-specifier %modifier-invocation
     #'(lambda (vir sta vis ove mod)
         `( :virtual ,vir  :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%state-mutability %visibility %override-specifier %modifier-invocation
     #'(lambda (sta vis ove mod)
         `( :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%virtual %state-mutability %visibility %modifier-invocation %override-specifier
     #'(lambda (vir sta vis mod ove)
         `( :virtual ,vir  :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%state-mutability %visibility %modifier-invocation %override-specifier
     #'(lambda (sta vis mod ove)
         `( :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%virtual %modifier-invocation %override-specifier %visibility
     #'(lambda (vir mod ove vis)
         `( :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis )))
   (%modifier-invocation %override-specifier %visibility
     #'(lambda (mod ove vis)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis )))
   (%virtual %state-mutability %modifier-invocation %override-specifier %visibility
     #'(lambda (vir sta mod ove vis)
         `( :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis )))
   (%state-mutability %modifier-invocation %override-specifier %visibility
     #'(lambda (sta mod ove vis)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove  :visibility ,vis )))
   (%virtual %modifier-invocation %visibility %override-specifier
     #'(lambda (vir mod vis ove)
         `( :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove )))
   (%modifier-invocation %visibility %override-specifier
     #'(lambda (mod vis ove)
         `( :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove )))
   (%virtual %state-mutability %modifier-invocation %visibility %override-specifier
     #'(lambda (vir sta mod vis ove)
         `( :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove )))
   (%state-mutability %modifier-invocation %visibility %override-specifier
     #'(lambda (sta mod vis ove)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis  :override-specifier ,ove )))
   (%virtual %state-mutability %visibility %override-specifier
     #'(lambda (vir sta vis ove)
         `( :virtual ,vir  :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove )))
   (%state-mutability %visibility %override-specifier
     #'(lambda (sta vis ove)
         `( :state-mutability ,sta  :visibility ,vis  :override-specifier ,ove )))
   (%virtual %override-specifier %visibility
     #'(lambda (vir ove vis)
         `( :virtual ,vir  :override-specifier ,ove  :visibility ,vis )))
   (%virtual %state-mutability %override-specifier %visibility
     #'(lambda (vir sta ove vis)
         `( :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis )))
   (%state-mutability %override-specifier %visibility
     #'(lambda (sta ove vis)
         `( :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis )))
   (%virtual %override-specifier %modifier-invocation %visibility
     #'(lambda (vir ove mod vis)
         `( :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis )))
   (%virtual %state-mutability %override-specifier %modifier-invocation %visibility
     #'(lambda (vir sta ove mod vis)
         `( :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis )))
   (%state-mutability %override-specifier %modifier-invocation %visibility
     #'(lambda (sta ove mod vis)
         `( :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis )))
   (%virtual %override-specifier %visibility %modifier-invocation
     #'(lambda (vir ove vis mod)
         `( :virtual ,vir  :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod )))
   (%virtual %state-mutability %override-specifier %visibility %modifier-invocation
     #'(lambda (vir sta ove vis mod)
         `( :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod )))
   (%state-mutability %override-specifier %visibility %modifier-invocation
     #'(lambda (sta ove vis mod)
         `( :state-mutability ,sta  :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod )))
   (%virtual %state-mutability %override-specifier %modifier-invocation
     #'(lambda (vir sta ove mod)
         `( :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%state-mutability %override-specifier %modifier-invocation
     #'(lambda (sta ove mod)
         `( :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%virtual %visibility %state-mutability %override-specifier %modifier-invocation
     #'(lambda (vir vis sta ove mod)
         `( :virtual ,vir  :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%visibility %state-mutability %override-specifier %modifier-invocation
     #'(lambda (vis sta ove mod)
         `( :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%virtual %state-mutability %modifier-invocation %override-specifier
     #'(lambda (vir sta mod ove)
         `( :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%state-mutability %modifier-invocation %override-specifier
     #'(lambda (sta mod ove)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%virtual %visibility %state-mutability %modifier-invocation %override-specifier
     #'(lambda (vir vis sta mod ove)
         `( :virtual ,vir  :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%visibility %state-mutability %modifier-invocation %override-specifier
     #'(lambda (vis sta mod ove)
         `( :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%virtual %modifier-invocation %override-specifier %state-mutability
     #'(lambda (vir mod ove sta)
         `( :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta )))
   (%modifier-invocation %override-specifier %state-mutability
     #'(lambda (mod ove sta)
         `( :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta )))
   (%virtual %visibility %modifier-invocation %override-specifier %state-mutability
     #'(lambda (vir vis mod ove sta)
         `( :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta )))
   (%visibility %modifier-invocation %override-specifier %state-mutability
     #'(lambda (vis mod ove sta)
         `( :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove  :state-mutability ,sta )))
   (%virtual %modifier-invocation %override-specifier
     #'(lambda (vir mod ove)
         `( :virtual ,vir  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%modifier-invocation %override-specifier
     #'(lambda (mod ove)
         `( :modifier-invocation ,mod  :override-specifier ,ove )))
   (%virtual %visibility %modifier-invocation %override-specifier
     #'(lambda (vir vis mod ove)
         `( :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%visibility %modifier-invocation %override-specifier
     #'(lambda (vis mod ove)
         `( :visibility ,vis  :modifier-invocation ,mod  :override-specifier ,ove )))
   (%virtual %override-specifier
     #'(lambda (vir ove)
         `( :virtual ,vir  :override-specifier ,ove )))
   (%override-specifier
     #'(lambda (ove)
         `( :override-specifier ,ove )))
   (%virtual %visibility %override-specifier
     #'(lambda (vir vis ove)
         `( :virtual ,vir  :visibility ,vis  :override-specifier ,ove )))
   (%visibility %override-specifier
     #'(lambda (vis ove)
         `( :visibility ,vis  :override-specifier ,ove )))
   (%virtual %modifier-invocation %state-mutability %override-specifier
     #'(lambda (vir mod sta ove)
         `( :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove )))
   (%modifier-invocation %state-mutability %override-specifier
     #'(lambda (mod sta ove)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove )))
   (%virtual %visibility %modifier-invocation %state-mutability %override-specifier
     #'(lambda (vir vis mod sta ove)
         `( :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove )))
   (%visibility %modifier-invocation %state-mutability %override-specifier
     #'(lambda (vis mod sta ove)
         `( :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta  :override-specifier ,ove )))
   (%virtual %state-mutability %override-specifier
     #'(lambda (vir sta ove)
         `( :virtual ,vir  :state-mutability ,sta  :override-specifier ,ove )))
   (%state-mutability %override-specifier
     #'(lambda (sta ove)
         `( :state-mutability ,sta  :override-specifier ,ove )))
   (%virtual %visibility %state-mutability %override-specifier
     #'(lambda (vir vis sta ove)
         `( :virtual ,vir  :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove )))
   (%visibility %state-mutability %override-specifier
     #'(lambda (vis sta ove)
         `( :visibility ,vis  :state-mutability ,sta  :override-specifier ,ove )))
   (%virtual %override-specifier %state-mutability
     #'(lambda (vir ove sta)
         `( :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta )))
   (%virtual %visibility %override-specifier %state-mutability
     #'(lambda (vir vis ove sta)
         `( :virtual ,vir  :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta )))
   (%visibility %override-specifier %state-mutability
     #'(lambda (vis ove sta)
         `( :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta )))
   (%virtual %override-specifier %modifier-invocation %state-mutability
     #'(lambda (vir ove mod sta)
         `( :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%virtual %visibility %override-specifier %modifier-invocation %state-mutability
     #'(lambda (vir vis ove mod sta)
         `( :virtual ,vir  :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%visibility %override-specifier %modifier-invocation %state-mutability
     #'(lambda (vis ove mod sta)
         `( :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%virtual %override-specifier %modifier-invocation
     #'(lambda (vir ove mod)
         `( :virtual ,vir  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%virtual %visibility %override-specifier %modifier-invocation
     #'(lambda (vir vis ove mod)
         `( :virtual ,vir  :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%visibility %override-specifier %modifier-invocation
     #'(lambda (vis ove mod)
         `( :visibility ,vis  :override-specifier ,ove  :modifier-invocation ,mod )))
   (%virtual %override-specifier %state-mutability %modifier-invocation
     #'(lambda (vir ove sta mod)
         `( :virtual ,vir  :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%virtual %visibility %override-specifier %state-mutability %modifier-invocation
     #'(lambda (vir vis ove sta mod)
         `( :virtual ,vir  :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%visibility %override-specifier %state-mutability %modifier-invocation
     #'(lambda (vis ove sta mod)
         `( :visibility ,vis  :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%override-specifier %virtual %visibility %modifier-invocation %state-mutability
     #'(lambda (ove vir vis mod sta)
         `( :override-specifier ,ove  :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%virtual %visibility %modifier-invocation %state-mutability
     #'(lambda (vir vis mod sta)
         `( :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%override-specifier %virtual %visibility %state-mutability %modifier-invocation
     #'(lambda (ove vir vis sta mod)
         `( :override-specifier ,ove  :virtual ,vir  :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%virtual %visibility %state-mutability %modifier-invocation
     #'(lambda (vir vis sta mod)
         `( :virtual ,vir  :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%override-specifier %virtual %state-mutability %modifier-invocation %visibility
     #'(lambda (ove vir sta mod vis)
         `( :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis )))
   (%virtual %state-mutability %modifier-invocation %visibility
     #'(lambda (vir sta mod vis)
         `( :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis )))
   (%override-specifier %virtual %state-mutability %visibility %modifier-invocation
     #'(lambda (ove vir sta vis mod)
         `( :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod )))
   (%virtual %state-mutability %visibility %modifier-invocation
     #'(lambda (vir sta vis mod)
         `( :virtual ,vir  :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod )))
   (%override-specifier %virtual %modifier-invocation %state-mutability %visibility
     #'(lambda (ove vir mod sta vis)
         `( :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis )))
   (%virtual %modifier-invocation %state-mutability %visibility
     #'(lambda (vir mod sta vis)
         `( :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis )))
   (%override-specifier %virtual %modifier-invocation %visibility %state-mutability
     #'(lambda (ove vir mod vis sta)
         `( :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta )))
   (%virtual %modifier-invocation %visibility %state-mutability
     #'(lambda (vir mod vis sta)
         `( :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta )))
   (%override-specifier %modifier-invocation %visibility %virtual %state-mutability
     #'(lambda (ove mod vis vir sta)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir  :state-mutability ,sta )))
   (%modifier-invocation %visibility %virtual %state-mutability
     #'(lambda (mod vis vir sta)
         `( :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir  :state-mutability ,sta )))
   (%override-specifier %modifier-invocation %visibility %state-mutability %virtual
     #'(lambda (ove mod vis sta vir)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta  :virtual ,vir )))
   (%modifier-invocation %visibility %state-mutability %virtual
     #'(lambda (mod vis sta vir)
         `( :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta  :virtual ,vir )))
   (%override-specifier %modifier-invocation %state-mutability %virtual %visibility
     #'(lambda (ove mod sta vir vis)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir  :visibility ,vis )))
   (%modifier-invocation %state-mutability %virtual %visibility
     #'(lambda (mod sta vir vis)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir  :visibility ,vis )))
   (%override-specifier %modifier-invocation %state-mutability %visibility %virtual
     #'(lambda (ove mod sta vis vir)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis  :virtual ,vir )))
   (%modifier-invocation %state-mutability %visibility %virtual
     #'(lambda (mod sta vis vir)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis  :virtual ,vir )))
   (%override-specifier %virtual %state-mutability %visibility
     #'(lambda (ove vir sta vis)
         `( :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta  :visibility ,vis )))
   (%virtual %state-mutability %visibility
     #'(lambda (vir sta vis)
         `( :virtual ,vir  :state-mutability ,sta  :visibility ,vis )))
   (%override-specifier %modifier-invocation %virtual %state-mutability %visibility
     #'(lambda (ove mod vir sta vis)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta  :visibility ,vis )))
   (%modifier-invocation %virtual %state-mutability %visibility
     #'(lambda (mod vir sta vis)
         `( :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta  :visibility ,vis )))
   (%override-specifier %modifier-invocation %state-mutability %visibility
     #'(lambda (ove mod sta vis)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis )))
   (%modifier-invocation %state-mutability %visibility
     #'(lambda (mod sta vis)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :visibility ,vis )))
   (%override-specifier %virtual %visibility %state-mutability
     #'(lambda (ove vir vis sta)
         `( :override-specifier ,ove  :virtual ,vir  :visibility ,vis  :state-mutability ,sta )))
   (%virtual %visibility %state-mutability
     #'(lambda (vir vis sta)
         `( :virtual ,vir  :visibility ,vis  :state-mutability ,sta )))
   (%override-specifier %modifier-invocation %virtual %visibility %state-mutability
     #'(lambda (ove mod vir vis sta)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis  :state-mutability ,sta )))
   (%modifier-invocation %virtual %visibility %state-mutability
     #'(lambda (mod vir vis sta)
         `( :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis  :state-mutability ,sta )))
   (%override-specifier %modifier-invocation %visibility %state-mutability
     #'(lambda (ove mod vis sta)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta )))
   (%modifier-invocation %visibility %state-mutability
     #'(lambda (mod vis sta)
         `( :modifier-invocation ,mod  :visibility ,vis  :state-mutability ,sta )))
   (%override-specifier %state-mutability %visibility %virtual %modifier-invocation
     #'(lambda (ove sta vis vir mod)
         `( :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod )))
   (%state-mutability %visibility %virtual %modifier-invocation
     #'(lambda (sta vis vir mod)
         `( :state-mutability ,sta  :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod )))
   (%override-specifier %state-mutability %visibility %modifier-invocation %virtual
     #'(lambda (ove sta vis mod vir)
         `( :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir )))
   (%state-mutability %visibility %modifier-invocation %virtual
     #'(lambda (sta vis mod vir)
         `( :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir )))
   (%override-specifier %modifier-invocation %virtual %visibility
     #'(lambda (ove mod vir vis)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis )))
   (%modifier-invocation %virtual %visibility
     #'(lambda (mod vir vis)
         `( :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis )))
   (%override-specifier %state-mutability %modifier-invocation %virtual %visibility
     #'(lambda (ove sta mod vir vis)
         `( :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis )))
   (%state-mutability %modifier-invocation %virtual %visibility
     #'(lambda (sta mod vir vis)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir  :visibility ,vis )))
   (%override-specifier %modifier-invocation %visibility %virtual
     #'(lambda (ove mod vis vir)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir )))
   (%modifier-invocation %visibility %virtual
     #'(lambda (mod vis vir)
         `( :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir )))
   (%override-specifier %state-mutability %modifier-invocation %visibility %virtual
     #'(lambda (ove sta mod vis vir)
         `( :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir )))
   (%state-mutability %modifier-invocation %visibility %virtual
     #'(lambda (sta mod vis vir)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis  :virtual ,vir )))
   (%override-specifier %state-mutability %visibility %virtual
     #'(lambda (ove sta vis vir)
         `( :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis  :virtual ,vir )))
   (%state-mutability %visibility %virtual
     #'(lambda (sta vis vir)
         `( :state-mutability ,sta  :visibility ,vis  :virtual ,vir )))
   (%override-specifier %virtual %visibility
     #'(lambda (ove vir vis)
         `( :override-specifier ,ove  :virtual ,vir  :visibility ,vis )))
   (%virtual %visibility
     #'(lambda (vir vis)
         `( :virtual ,vir  :visibility ,vis )))
   (%override-specifier %state-mutability %virtual %visibility
     #'(lambda (ove sta vir vis)
         `( :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir  :visibility ,vis )))
   (%state-mutability %virtual %visibility
     #'(lambda (sta vir vis)
         `( :state-mutability ,sta  :virtual ,vir  :visibility ,vis )))
   (%override-specifier %visibility
     #'(lambda (ove vis)
         `( :override-specifier ,ove  :visibility ,vis )))
   (%visibility
     #'(lambda (vis)
         `( :visibility ,vis )))
   (%override-specifier %state-mutability %visibility
     #'(lambda (ove sta vis)
         `( :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis )))
   (%state-mutability %visibility
     #'(lambda (sta vis)
         `( :state-mutability ,sta  :visibility ,vis )))
   (%override-specifier %virtual %modifier-invocation %visibility
     #'(lambda (ove vir mod vis)
         `( :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis )))
   (%virtual %modifier-invocation %visibility
     #'(lambda (vir mod vis)
         `( :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis )))
   (%override-specifier %state-mutability %virtual %modifier-invocation %visibility
     #'(lambda (ove sta vir mod vis)
         `( :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis )))
   (%state-mutability %virtual %modifier-invocation %visibility
     #'(lambda (sta vir mod vis)
         `( :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod  :visibility ,vis )))
   (%override-specifier %modifier-invocation %visibility
     #'(lambda (ove mod vis)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :visibility ,vis )))
   (%modifier-invocation %visibility
     #'(lambda (mod vis)
         `( :modifier-invocation ,mod  :visibility ,vis )))
   (%override-specifier %state-mutability %modifier-invocation %visibility
     #'(lambda (ove sta mod vis)
         `( :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis )))
   (%state-mutability %modifier-invocation %visibility
     #'(lambda (sta mod vis)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :visibility ,vis )))
   (%override-specifier %virtual %visibility %modifier-invocation
     #'(lambda (ove vir vis mod)
         `( :override-specifier ,ove  :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod )))
   (%virtual %visibility %modifier-invocation
     #'(lambda (vir vis mod)
         `( :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod )))
   (%override-specifier %state-mutability %virtual %visibility %modifier-invocation
     #'(lambda (ove sta vir vis mod)
         `( :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod )))
   (%state-mutability %virtual %visibility %modifier-invocation
     #'(lambda (sta vir vis mod)
         `( :state-mutability ,sta  :virtual ,vir  :visibility ,vis  :modifier-invocation ,mod )))
   (%override-specifier %state-mutability %visibility %modifier-invocation
     #'(lambda (ove sta vis mod)
         `( :override-specifier ,ove  :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod )))
   (%state-mutability %visibility %modifier-invocation
     #'(lambda (sta vis mod)
         `( :state-mutability ,sta  :visibility ,vis  :modifier-invocation ,mod )))
   (%override-specifier %state-mutability %virtual %modifier-invocation
     #'(lambda (ove sta vir mod)
         `( :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod )))
   (%state-mutability %virtual %modifier-invocation
     #'(lambda (sta vir mod)
         `( :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod )))
   (%override-specifier %visibility %state-mutability %virtual %modifier-invocation
     #'(lambda (ove vis sta vir mod)
         `( :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod )))
   (%visibility %state-mutability %virtual %modifier-invocation
     #'(lambda (vis sta vir mod)
         `( :visibility ,vis  :state-mutability ,sta  :virtual ,vir  :modifier-invocation ,mod )))
   (%override-specifier %state-mutability %modifier-invocation %virtual
     #'(lambda (ove sta mod vir)
         `( :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir )))
   (%state-mutability %modifier-invocation %virtual
     #'(lambda (sta mod vir)
         `( :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir )))
   (%override-specifier %visibility %state-mutability %modifier-invocation %virtual
     #'(lambda (ove vis sta mod vir)
         `( :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir )))
   (%visibility %state-mutability %modifier-invocation %virtual
     #'(lambda (vis sta mod vir)
         `( :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod  :virtual ,vir )))
   (%override-specifier %modifier-invocation %virtual %state-mutability
     #'(lambda (ove mod vir sta)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta )))
   (%modifier-invocation %virtual %state-mutability
     #'(lambda (mod vir sta)
         `( :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta )))
   (%override-specifier %visibility %modifier-invocation %virtual %state-mutability
     #'(lambda (ove vis mod vir sta)
         `( :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta )))
   (%visibility %modifier-invocation %virtual %state-mutability
     #'(lambda (vis mod vir sta)
         `( :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir  :state-mutability ,sta )))
   (%override-specifier %modifier-invocation %virtual
     #'(lambda (ove mod vir)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :virtual ,vir )))
   (%modifier-invocation %virtual
     #'(lambda (mod vir)
         `( :modifier-invocation ,mod  :virtual ,vir )))
   (%override-specifier %visibility %modifier-invocation %virtual
     #'(lambda (ove vis mod vir)
         `( :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir )))
   (%visibility %modifier-invocation %virtual
     #'(lambda (vis mod vir)
         `( :visibility ,vis  :modifier-invocation ,mod  :virtual ,vir )))
   (%override-specifier %virtual
     #'(lambda (ove vir)
         `( :override-specifier ,ove  :virtual ,vir )))
   (%virtual
     #'(lambda (vir)
         `( :virtual ,vir )))
   (%override-specifier %visibility %virtual
     #'(lambda (ove vis vir)
         `( :override-specifier ,ove  :visibility ,vis  :virtual ,vir )))
   (%visibility %virtual
     #'(lambda (vis vir)
         `( :visibility ,vis  :virtual ,vir )))
   (%override-specifier %modifier-invocation %state-mutability %virtual
     #'(lambda (ove mod sta vir)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir )))
   (%modifier-invocation %state-mutability %virtual
     #'(lambda (mod sta vir)
         `( :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir )))
   (%override-specifier %visibility %modifier-invocation %state-mutability %virtual
     #'(lambda (ove vis mod sta vir)
         `( :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir )))
   (%visibility %modifier-invocation %state-mutability %virtual
     #'(lambda (vis mod sta vir)
         `( :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta  :virtual ,vir )))
   (%override-specifier %state-mutability %virtual
     #'(lambda (ove sta vir)
         `( :override-specifier ,ove  :state-mutability ,sta  :virtual ,vir )))
   (%state-mutability %virtual
     #'(lambda (sta vir)
         `( :state-mutability ,sta  :virtual ,vir )))
   (%override-specifier %visibility %state-mutability %virtual
     #'(lambda (ove vis sta vir)
         `( :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta  :virtual ,vir )))
   (%visibility %state-mutability %virtual
     #'(lambda (vis sta vir)
         `( :visibility ,vis  :state-mutability ,sta  :virtual ,vir )))
   (%override-specifier %virtual %state-mutability
     #'(lambda (ove vir sta)
         `( :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta )))
   (%virtual %state-mutability
     #'(lambda (vir sta)
         `( :virtual ,vir  :state-mutability ,sta )))
   (%override-specifier %visibility %virtual %state-mutability
     #'(lambda (ove vis vir sta)
         `( :override-specifier ,ove  :visibility ,vis  :virtual ,vir  :state-mutability ,sta )))
   (%visibility %virtual %state-mutability
     #'(lambda (vis vir sta)
         `( :visibility ,vis  :virtual ,vir  :state-mutability ,sta )))
   (%override-specifier %state-mutability
     #'(lambda (ove sta)
         `( :override-specifier ,ove  :state-mutability ,sta )))
   (%state-mutability
     #'(lambda (sta)
         `( :state-mutability ,sta )))
   (%override-specifier %visibility %state-mutability
     #'(lambda (ove vis sta)
         `( :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta )))
   (%visibility %state-mutability
     #'(lambda (vis sta)
         `( :visibility ,vis  :state-mutability ,sta )))
   (%override-specifier %virtual %modifier-invocation %state-mutability
     #'(lambda (ove vir mod sta)
         `( :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%virtual %modifier-invocation %state-mutability
     #'(lambda (vir mod sta)
         `( :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%override-specifier %visibility %virtual %modifier-invocation %state-mutability
     #'(lambda (ove vis vir mod sta)
         `( :override-specifier ,ove  :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%visibility %virtual %modifier-invocation %state-mutability
     #'(lambda (vis vir mod sta)
         `( :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%override-specifier %modifier-invocation %state-mutability
     #'(lambda (ove mod sta)
         `( :override-specifier ,ove  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%modifier-invocation %state-mutability
     #'(lambda (mod sta)
         `( :modifier-invocation ,mod  :state-mutability ,sta )))
   (%override-specifier %visibility %modifier-invocation %state-mutability
     #'(lambda (ove vis mod sta)
         `( :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%visibility %modifier-invocation %state-mutability
     #'(lambda (vis mod sta)
         `( :visibility ,vis  :modifier-invocation ,mod  :state-mutability ,sta )))
   (%override-specifier %virtual %modifier-invocation
     #'(lambda (ove vir mod)
         `( :override-specifier ,ove  :virtual ,vir  :modifier-invocation ,mod )))
   (%virtual %modifier-invocation
     #'(lambda (vir mod)
         `( :virtual ,vir  :modifier-invocation ,mod )))
   (%override-specifier %visibility %virtual %modifier-invocation
     #'(lambda (ove vis vir mod)
         `( :override-specifier ,ove  :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod )))
   (%visibility %virtual %modifier-invocation
     #'(lambda (vis vir mod)
         `( :visibility ,vis  :virtual ,vir  :modifier-invocation ,mod )))
   (%override-specifier %modifier-invocation
     #'(lambda (ove mod)
         `( :override-specifier ,ove  :modifier-invocation ,mod )))
   (%modifier-invocation
     #'(lambda (mod)
         `( :modifier-invocation ,mod )))
   (%override-specifier %visibility %modifier-invocation
     #'(lambda (ove vis mod)
         `( :override-specifier ,ove  :visibility ,vis  :modifier-invocation ,mod )))
   (%visibility %modifier-invocation
     #'(lambda (vis mod)
         `( :visibility ,vis  :modifier-invocation ,mod )))
   (%override-specifier %virtual %state-mutability %modifier-invocation
     #'(lambda (ove vir sta mod)
         `( :override-specifier ,ove  :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%virtual %state-mutability %modifier-invocation
     #'(lambda (vir sta mod)
         `( :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%override-specifier %visibility %virtual %state-mutability %modifier-invocation
     #'(lambda (ove vis vir sta mod)
         `( :override-specifier ,ove  :visibility ,vis  :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%visibility %virtual %state-mutability %modifier-invocation
     #'(lambda (vis vir sta mod)
         `( :visibility ,vis  :virtual ,vir  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%override-specifier %state-mutability %modifier-invocation
     #'(lambda (ove sta mod)
         `( :override-specifier ,ove  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%state-mutability %modifier-invocation
     #'(lambda (sta mod)
         `( :state-mutability ,sta  :modifier-invocation ,mod )))
   (%override-specifier %visibility %state-mutability %modifier-invocation
     #'(lambda (ove vis sta mod)
         `( :override-specifier ,ove  :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod )))
   (%visibility %state-mutability %modifier-invocation
     #'(lambda (vis sta mod)
         `( :visibility ,vis  :state-mutability ,sta  :modifier-invocation ,mod )))
   
   )
  (%state-mutability
   (%pure    #'(lambda (x) `(:state-mutability ,x)))
   (%view    #'(lambda (x) `(:state-mutability ,x)))
   (%payable #'(lambda (x) `(:state-mutability ,x)))
   )
  (%modifier-invocation
   (%ident-path #'(lambda (ip) `(:ident-path ,ip)))
   (%ident-path %call-arg-list
                #'(lambda (ip args) `(:id-path ,ip :args ,args)))
   )
  (%override-specifier
   (%override #'(lambda (ovr) `(:override nil)))
   (%override |%(| %multi-ident-path |%)| #'(lambda (ovr lb mip rb) `(:override ,mip)))
   )
  (%ident-path
   (%ident #'(lambda (last) `(:ident (,last))))
   (%ident |%.| %ident-path
           #'(lambda (head dot rest)
               `(:ident ,(append (list head) (cadr rest)))))
   )
  (%ident
   (%identifier #'(lambda (id) `(:id ,id)))
   (%from #'(lambda (fr) `(:from ,fr)))
   ;; (%error #'(lambda (er) `(:error ,er)))
   ;; (%revert #'(lambda (rv) `(:revert ,rv)))
   )
  (%multi-ident-path
   (%ident-path #'(lambda (id) `(:id ,id)))
   (%ident-path |%,| %multi-ident-path #'(lambda (id cm mip) `(:id-head ,id :id-rest ,mip)))
   )
  (%type-name
   (%elt-type-name #'(lambda (par) `(:elt-type-name ,par)))
   (%func-type-name #'(lambda (par) `(:func-type-name ,par)))
   (%mapping-type #'(lambda (par) `(:mapping-type ,par)))
   (%type-name %inline-array-expr
               #'(lambda (par inline-expr) `(:type-name ,par :type-name-expr ,inline-expr)))
   (%ident-path #'(lambda (par) `(:identifier-path ,par)))
   )
  
  (%func-type-name
   (%function |%(| |%)|
              #'(lambda (fu lb rb)
                  `(:func-type-name nil)))
   (%function |%(| %param-list |%)|
              #'(lambda (fu lb pl rb)
                  `(:func-type-name nil :param-list ,pl)))
   (%function |%(| |%)| %visibility
              #'(lambda (fu lb rb vi)
                  `(:func-type-name nil :visibility ,vi)))
   (%function |%(| %param-list |%)| %visibility
              #'(lambda (fu lb pl rb vi)
                  `(:func-type-name nil :param-list ,pl :visibility ,vi)))
   (%function |%(| |%)| %state-mutability
              #'(lambda (fu lb rb sm)
                  `(:func-type-name nil :state-mutability ,sm)))
   (%function |%(| %param-list |%)| %state-mutability
              #'(lambda (fu lb pl rb sm)
                  `(:func-type-name nil :param-list ,pl :state-mutability ,sm)))
   (%function |%(| |%)| %visibility %state-mutability
              #'(lambda (fu lb rb vi sm)
                  `(:func-type-name nil :visibility ,vi :state-mutability ,sm)))
   (%function |%(| %param-list |%)| %visibility %state-mutability
              #'(lambda (fu lb pl rb vi sm)
                  `(:func-type-name nil :param-list ,pl :visibility ,vi :state-mutability ,sm)))
   (%function |%(| |%)| %state-mutability %visibility
              #'(lambda (fu lb rb sm vi)
                  `(:func-type-name nil :visibility ,vi :state-mutability ,sm)))
   (%function |%(| %param-list |%)| %state-mutability %visibility
              #'(lambda (fu lb pl rb sm vi)
                  `(:func-type-name nil :param-list ,pl :visibility ,vi :state-mutability ,sm)))
  
   (%function |%(| |%)| %returns |%(| %param-list |%)|
              #'(lambda (fu lb rb rt lb2 pl2 rb2)
                  `(:func-type-name nil
                    :returns ,pl2)))
   (%function |%(| %param-list |%)| %returns |%(| %param-list |%)|
              #'(lambda (fu lb pl rb rt lb2 pl2 rb2)
                  `(:func-type-name nil :param-list ,pl
                    :returns ,pl2)))
   (%function |%(| |%)| %visibility %returns |%(| %param-list |%)|
              #'(lambda (fu lb rb vi rt lb2 pl2 rb2)
                  `(:func-type-name nil :visibility ,vi
                    :returns ,pl2)))
   (%function |%(| %param-list |%)| %visibility %returns |%(| %param-list |%)|
              #'(lambda (fu lb pl rb vi rt lb2 pl2 rb2)
                  `(:func-type-name nil :param-list ,pl :visibility ,vi
                    :returns ,pl2)))
   (%function |%(| |%)| %state-mutability %returns |%(| %param-list |%)|
              #'(lambda (fu lb rb sm rt lb2 pl2 rb2)
                  `(:func-type-name nil :state-mutability ,sm
                    :returns ,pl2)))
   (%function |%(| %param-list |%)| %state-mutability %returns |%(| %param-list |%)|
              #'(lambda (fu lb pl rb sm rt lb2 pl2 rb2)
                  `(:func-type-name nil :param-list ,pl :state-mutability ,sm
                    :returns ,pl2)))
   (%function |%(| |%)| %visibility %state-mutability %returns |%(| %param-list |%)|
              #'(lambda (fu lb rb vi sm rt lb2 pl2 rb2)
                  `(:func-type-name nil :visibility ,vi :state-mutability ,sm
                    :returns ,pl2)))
   (%function |%(| %param-list |%)| %visibility %state-mutability %returns |%(| %param-list |%)|
              #'(lambda (fu lb pl rb vi sm rt lb2 pl2 rb2)
                  `(:func-type-name nil :param-list ,pl :visibility ,vi :state-mutability ,sm
                    :returns ,pl2)))
   (%function |%(| |%)| %state-mutability %visibility %returns |%(| %param-list |%)|
              #'(lambda (fu lb rb sm vi rt lb2 pl2 rb2)
                  `(:func-type-name nil :visibility ,vi :state-mutability ,sm
                    :returns ,pl2)))
   (%function |%(| %param-list |%)| %state-mutability %visibility %returns |%(| %param-list |%)|
              #'(lambda (fu lb pl rb sm vi rt lb2 pl2 rb2)
                  `(:func-type-name nil :param-list ,pl :visibility ,vi :state-mutability ,sm
                    :returns ,pl2)))
  )
  (%mapping-type
   (%mapping |%(| %mapping-key-type |%=>| %type-name |%)|
             #'(lambda (op lb mkt to tn rb)
                 `(:mapping ,mkt :type ,tn)))
   )
  (%mapping-key-type
   (%elt-type-name #'(lambda (type) `(:elt_type ,type)))
   (%ident-path #'(lambda (ip) `(:ident-path ,ip)))
   )
  (%elt-type-name
   (%address #'(lambda (ad) `(:address ad)))
   (%address %payable #'(lambda (ad pa) `(:address-payable nil)))
   (%bool #'(lambda (par) `(:bool ,par)))
   (%string #'(lambda (par) `(:string ,par)))
   (%bytes  #'(lambda (par) `(:bytes  ,par)))
   (%signed-integer-type #'(lambda (par) `(:signed-integer-type ,par)))
   (%unsigned-integer-type #'(lambda (par) `(:unsigned-integer-type ,par)))
   (%fixed-bytes #'(lambda (par) `(:fixed-bytes ,par)))
   (%fixed #'(lambda (par) `(:fixed ,par)))
   (%ufixed #'(lambda (par) `(:fixed ,par)))
   )
  (%block
   (|%;| #'(lambda (sc) `(:non-blk nil)))
   (|%{| |%}| #'(lambda (l-brak r-brak) `(:empty-blk nil)))
   (|%{| %stmnt |%}| #'(lambda (l-brak stmnt r-brak) `(:stmnt ,stmnt)))
   (|%{| %unchecked-block-contents |%}|
         #'(lambda (l-brak cont r-brak) `(:unchecked-blk-contents ,cont)))
   )
  (%unchecked-block-contents
   (%unchecked-block #'(lambda (last) `(:unchk-blk (,last))))
   (%unchecked-block %unchecked-block-contents
                     #'(lambda (head rest)
                         `(:unchk-blk ,(append (list head) (cadr rest)))))
   )
  (%unchecked-block
   (%unchecked %block #'(lambda (a b) `(:unchecked-block ,b)))
   )
  (%ctor-def
   (%constructor %parlist %block
                 #'(lambda (ct pl blk)
                     `(:constructor nil :parlist ,pl :block ,blk)))
   (%constructor %parlist %cmeta %block
                 #'(lambda (ct pl cm blk)
                     `(:constructor nil :parlist ,pl :cmeta ,cm :block ,blk)))
   )
  (%cmeta
   (%modifier-invocation %visibility %payable %internal
     #'(lambda (mod vis pay int)
         `( :modifier-invocation ,mod  :visibility ,vis  :payable ,pay  :internal ,int )))
   (%modifier-invocation %visibility %internal %payable
     #'(lambda (mod vis int pay)
         `( :modifier-invocation ,mod  :visibility ,vis  :internal ,int  :payable ,pay )))
   (%modifier-invocation %internal %payable %visibility
     #'(lambda (mod int pay vis)
         `( :modifier-invocation ,mod  :internal ,int  :payable ,pay  :visibility ,vis )))
   (%modifier-invocation %internal %visibility %payable
     #'(lambda (mod int vis pay)
         `( :modifier-invocation ,mod  :internal ,int  :visibility ,vis  :payable ,pay )))
   (%modifier-invocation %payable %internal %visibility
     #'(lambda (mod pay int vis)
         `( :modifier-invocation ,mod  :payable ,pay  :internal ,int  :visibility ,vis )))
   (%modifier-invocation %payable %visibility %internal
     #'(lambda (mod pay vis int)
         `( :modifier-invocation ,mod  :payable ,pay  :visibility ,vis  :internal ,int )))
   (%payable %visibility %modifier-invocation %internal
     #'(lambda (pay vis mod int)
         `( :payable ,pay  :visibility ,vis  :modifier-invocation ,mod  :internal ,int )))
   (%payable %visibility %internal %modifier-invocation
     #'(lambda (pay vis int mod)
         `( :payable ,pay  :visibility ,vis  :internal ,int  :modifier-invocation ,mod )))
   (%payable %internal %modifier-invocation %visibility
     #'(lambda (pay int mod vis)
         `( :payable ,pay  :internal ,int  :modifier-invocation ,mod  :visibility ,vis )))
   (%payable %internal %visibility %modifier-invocation
     #'(lambda (pay int vis mod)
         `( :payable ,pay  :internal ,int  :visibility ,vis  :modifier-invocation ,mod )))
   (%payable %internal %visibility
     #'(lambda (pay int vis)
         `( :payable ,pay  :internal ,int  :visibility ,vis )))
   (%payable %modifier-invocation %internal %visibility
     #'(lambda (pay mod int vis)
         `( :payable ,pay  :modifier-invocation ,mod  :internal ,int  :visibility ,vis )))
   (%modifier-invocation %internal %visibility
     #'(lambda (mod int vis)
         `( :modifier-invocation ,mod  :internal ,int  :visibility ,vis )))
   (%payable %visibility %internal
     #'(lambda (pay vis int)
         `( :payable ,pay  :visibility ,vis  :internal ,int )))
   (%payable %modifier-invocation %visibility %internal
     #'(lambda (pay mod vis int)
         `( :payable ,pay  :modifier-invocation ,mod  :visibility ,vis  :internal ,int )))
   (%modifier-invocation %visibility %internal
     #'(lambda (mod vis int)
         `( :modifier-invocation ,mod  :visibility ,vis  :internal ,int )))
   (%internal %visibility %modifier-invocation %payable
     #'(lambda (int vis mod pay)
         `( :internal ,int  :visibility ,vis  :modifier-invocation ,mod  :payable ,pay )))
   (%internal %visibility %payable %modifier-invocation
     #'(lambda (int vis pay mod)
         `( :internal ,int  :visibility ,vis  :payable ,pay  :modifier-invocation ,mod )))
   (%internal %payable %modifier-invocation %visibility
     #'(lambda (int pay mod vis)
         `( :internal ,int  :payable ,pay  :modifier-invocation ,mod  :visibility ,vis )))
   (%payable %modifier-invocation %visibility
     #'(lambda (pay mod vis)
         `( :payable ,pay  :modifier-invocation ,mod  :visibility ,vis )))
   (%internal %visibility %modifier-invocation
     #'(lambda (int vis mod)
         `( :internal ,int  :visibility ,vis  :modifier-invocation ,mod )))
   (%internal %payable %visibility %modifier-invocation
     #'(lambda (int pay vis mod)
         `( :internal ,int  :payable ,pay  :visibility ,vis  :modifier-invocation ,mod )))
   (%payable %visibility %modifier-invocation
     #'(lambda (pay vis mod)
         `( :payable ,pay  :visibility ,vis  :modifier-invocation ,mod )))
   (%internal %payable %visibility
     #'(lambda (int pay vis)
         `( :internal ,int  :payable ,pay  :visibility ,vis )))
   (%payable %visibility
     #'(lambda (pay vis)
         `( :payable ,pay  :visibility ,vis )))
   (%internal %modifier-invocation %payable %visibility
     #'(lambda (int mod pay vis)
         `( :internal ,int  :modifier-invocation ,mod  :payable ,pay  :visibility ,vis )))
   (%modifier-invocation %payable %visibility
     #'(lambda (mod pay vis)
         `( :modifier-invocation ,mod  :payable ,pay  :visibility ,vis )))
   (%internal %visibility
     #'(lambda (int vis)
         `( :internal ,int  :visibility ,vis )))
   (%visibility
     #'(lambda (vis)
         `( :visibility ,vis )))
   (%internal %modifier-invocation %visibility
     #'(lambda (int mod vis)
         `( :internal ,int  :modifier-invocation ,mod  :visibility ,vis )))
   (%modifier-invocation %visibility
     #'(lambda (mod vis)
         `( :modifier-invocation ,mod  :visibility ,vis )))
   (%internal %visibility %payable
     #'(lambda (int vis pay)
         `( :internal ,int  :visibility ,vis  :payable ,pay )))
   (%internal %modifier-invocation %visibility %payable
     #'(lambda (int mod vis pay)
         `( :internal ,int  :modifier-invocation ,mod  :visibility ,vis  :payable ,pay )))
   (%modifier-invocation %visibility %payable
     #'(lambda (mod vis pay)
         `( :modifier-invocation ,mod  :visibility ,vis  :payable ,pay )))
   (%visibility %internal %modifier-invocation %payable
     #'(lambda (vis int mod pay)
         `( :visibility ,vis  :internal ,int  :modifier-invocation ,mod  :payable ,pay )))
   (%internal %modifier-invocation %payable
     #'(lambda (int mod pay)
         `( :internal ,int  :modifier-invocation ,mod  :payable ,pay )))
   (%visibility %internal %payable %modifier-invocation
     #'(lambda (vis int pay mod)
         `( :visibility ,vis  :internal ,int  :payable ,pay  :modifier-invocation ,mod )))
   (%internal %payable %modifier-invocation
     #'(lambda (int pay mod)
         `( :internal ,int  :payable ,pay  :modifier-invocation ,mod )))
   (%visibility %payable %modifier-invocation %internal
     #'(lambda (vis pay mod int)
         `( :visibility ,vis  :payable ,pay  :modifier-invocation ,mod  :internal ,int )))
   (%payable %modifier-invocation %internal
     #'(lambda (pay mod int)
         `( :payable ,pay  :modifier-invocation ,mod  :internal ,int )))
   (%visibility %internal %modifier-invocation
     #'(lambda (vis int mod)
         `( :visibility ,vis  :internal ,int  :modifier-invocation ,mod )))
   (%internal %modifier-invocation
     #'(lambda (int mod)
         `( :internal ,int  :modifier-invocation ,mod )))
   (%visibility %payable %internal %modifier-invocation
     #'(lambda (vis pay int mod)
         `( :visibility ,vis  :payable ,pay  :internal ,int  :modifier-invocation ,mod )))
   (%payable %internal %modifier-invocation
     #'(lambda (pay int mod)
         `( :payable ,pay  :internal ,int  :modifier-invocation ,mod )))
   (%visibility %modifier-invocation
     #'(lambda (vis mod)
         `( :visibility ,vis  :modifier-invocation ,mod )))
   (%modifier-invocation
     #'(lambda (mod)
         `( :modifier-invocation ,mod )))
   (%visibility %payable %modifier-invocation
     #'(lambda (vis pay mod)
         `( :visibility ,vis  :payable ,pay  :modifier-invocation ,mod )))
   (%payable %modifier-invocation
     #'(lambda (pay mod)
         `( :payable ,pay  :modifier-invocation ,mod )))
   (%visibility %payable %internal
     #'(lambda (vis pay int)
         `( :visibility ,vis  :payable ,pay  :internal ,int )))
   (%payable %internal
     #'(lambda (pay int)
         `( :payable ,pay  :internal ,int )))
   (%visibility %modifier-invocation %payable %internal
     #'(lambda (vis mod pay int)
         `( :visibility ,vis  :modifier-invocation ,mod  :payable ,pay  :internal ,int )))
   (%modifier-invocation %payable %internal
     #'(lambda (mod pay int)
         `( :modifier-invocation ,mod  :payable ,pay  :internal ,int )))
   (%visibility %internal
     #'(lambda (vis int)
         `( :visibility ,vis  :internal ,int )))
   (%internal
     #'(lambda (int)
         `( :internal ,int )))
   (%visibility %modifier-invocation %internal
     #'(lambda (vis mod int)
         `( :visibility ,vis  :modifier-invocation ,mod  :internal ,int )))
   (%modifier-invocation %internal
     #'(lambda (mod int)
         `( :modifier-invocation ,mod  :internal ,int )))
   (%visibility %internal %payable
     #'(lambda (vis int pay)
         `( :visibility ,vis  :internal ,int  :payable ,pay )))
   (%internal %payable
     #'(lambda (int pay)
         `( :internal ,int  :payable ,pay )))
   (%visibility %modifier-invocation %internal %payable
     #'(lambda (vis mod int pay)
         `( :visibility ,vis  :modifier-invocation ,mod  :internal ,int  :payable ,pay )))
   (%modifier-invocation %internal %payable
     #'(lambda (mod int pay)
         `( :modifier-invocation ,mod  :internal ,int  :payable ,pay )))
   (%visibility %payable
     #'(lambda (vis pay)
         `( :visibility ,vis  :payable ,pay )))
   (%payable
     #'(lambda (pay)
         `( :payable ,pay )))
   (%visibility %modifier-invocation %payable
     #'(lambda (vis mod pay)
         `( :visibility ,vis  :modifier-invocation ,mod  :payable ,pay )))
   (%modifier-invocation %payable
     #'(lambda (mod pay)
         `( :modifier-invocation ,mod  :payable ,pay )))
   
   )
  (%iface-def
   (%interface %identifier |%{| |%}|
               #'(lambda (in id lb rb) `(:iface ,id)))
   (%interface %identifier %is %inher-spec-contents |%{| |%}|
               #'(lambda (in id is isp lb rb) `(:iface ,id :is ,isp)))
   (%interface %identifier |%{| %ctract-body-elt-contents |%}|
               #'(lambda (in id lb bdy rb) `(:iface ,id :contents ,bdy)))
   (%interface %identifier %is %inher-spec-contents |%{|
               %ctract-body-elt-contents |%}|
               #'(lambda (in id is isp lb bdy rb)
                   `(:iface ,id :is ,isp :contents ,bdy)))
  )
  (%modif-def
   (%modifier %identifier |%;|
              #'(lambda (mo id cm) `(:mod-def ,id)))
   (%modifier %identifier %parlist |%;|
              #'(lambda (mo id pl cm) `(:mod-def ,id :parlist ,pl)))
   (%modifier %identifier %mdmeta |%;|
              #'(lambda (mo id me cm) `(:mod-def ,id :meta ,me)))
   (%modifier %identifier %parlist %mdmeta |%;|
              #'(lambda (mo id pl me cm) `(:mod-def ,id :parlist ,pl :meta ,me)))
  
   (%modifier %identifier %block
              #'(lambda (mo id blk) `(:mod-def ,id :blk ,blk)))
   (%modifier %identifier %parlist %block
              #'(lambda (mo id pl blk) `(:mod-def ,id :parlist ,pl :blk ,blk)))
   (%modifier %identifier %mdmeta %block
              #'(lambda (mo id me blk) `(:mod-def ,id :meta ,me :blk ,blk)))
   (%modifier %identifier %parlist %mdmeta %block
              #'(lambda (mo id pl me blk)
                  `(:mod-def ,id :parlist ,pl :meta ,me :blk ,blk)))
   )
  (%mdmeta
   (%virtual %override-specifier
     #'(lambda (vir ove)
         `( :virtual ,vir  :override-specifier ,ove )))
   (%override-specifier
     #'(lambda (ove)
         `( :override-specifier ,ove )))
   (%override-specifier %virtual
     #'(lambda (ove vir)
         `( :override-specifier ,ove  :virtual ,vir )))
   (%virtual
     #'(lambda (vir)
         `( :virtual ,vir )))
   
   )
  (%fallback-func-def
   (%fallback %parlist %block
              #'(lambda (fb parlist blk)
                  `(:fallback nil :parlist ,parlist :block ,blk)))
   (%fallback %parlist %retlist %block
              #'(lambda (fb id pl rl blk)
                  `(:fallback nil :parlist ,pl :retlist ,rl :block ,blk)))
   (%fallback %parlist %fmeta %retlist %block
              #'(lambda (fb parlist fmeta retlist blk)
                  `(:fallback nil :parlist ,parlist :fmeta ,fmeta :retlist ,retlist
                    :block ,blk)))
   (%fallback %parlist %fmeta %block
              #'(lambda (fb parlist fmeta blk)
                  `(:fallback nil :parlist ,parlist :fmeta ,fmeta :block ,blk)))
   )
  (%receive-func-def
   (%receive %parlist %block
              #'(lambda (rv parlist blk)
                  `(:receive nil :parlist ,parlist :block ,blk)))
   (%receive %parlist %retlist %block
              #'(lambda (rv id pl rl blk)
                  `(:receive nil :parlist ,pl :retlist ,rl :block ,blk)))
   (%receive %parlist %fmeta %retlist %block
              #'(lambda (rv parlist fmeta retlist blk)
                  `(:receive nil :parlist ,parlist :fmeta ,fmeta :retlist ,retlist
                    :block ,blk)))
   (%receive %parlist %fmeta %block
              #'(lambda (rv parlist fmeta blk)
                  `(:receive nil :parlist ,parlist :fmeta ,fmeta :block ,blk)))
   )
  (%struct-def
   (%struct %identifier |%{| %struct-member-contents |%}|
            #'(lambda (st id lb smc rb)
                `(:struct ,id :contents ,smc)))
   )
  (%struct-member-contents
   (%struct-member #'(lambda (last)
                       `(:member (,last))))
   (%struct-member %struct-member-contents
                   #'(lambda (head rest)
                       `(:member ,(append (list head) (cadr rest)))))
   )
  (%struct-member
   (%type-name %identifier |%;|
               #'(lambda (tn id sc)
                   `(:struct-name ,id :type ,tn)))
   )
  (%user-val-type-def
   (%type %identifier %is %elt-type-name |%;|
            #'(lambda (ty id is etn sc)
                `(:user-def-type ,id :def ,etn)))
   )
  (%error-def
   (%error %identifier |%(| |%)| |%;|
           #'(lambda (er id lb rb cm)
               `(:error-def ,id )))
   (%error %identifier |%(| %err-param-contents |%)| |%;|
           #'(lambda (er id lb mip rb cm)
               `(:error-def ,id :contents ,mip)))
   )
  (%err-param-contents
   (%err-param #'(lambda (last) `(:err-par (,last))))
   (%err-param |%,| %err-param-contents
               #'(lambda (head cm rest)
                   `(:err-par ,(append (list head) (cadr rest)))))
   )
  (%err-param
   (%type-name #'(lambda (tn) `(:err-type ,tn)))
   (%type-name %identifier #'(lambda (tn id) `(:err-type ,tn :err-name ,id)))
   )
  (%state-var-decl
   (%type-name %identifier |%;|
               #'(lambda (tn id sc)
                   `(:state-var-decl ,id :type-name ,tn)))
   (%type-name %identifier |%=| %expr |%;|
               #'(lambda (tn id eq ex sc)
                   `(:state-var-decl ,id :type-name ,tn :initval ,ex)))
   (%type-name %vmeta %identifier |%;|
               #'(lambda (tn ak id sc)
                   `(:const-var-decl ,id :type-name ,tn :access ,ak)))
   (%type-name %vmeta %identifier |%=| %expr |%;|
               #'(lambda (tn ak id rq ex sc)
                   `(:const-var-decl ,id :type-name ,tn :val ,ex :access ,ak)))
   )
  (%vmeta
   (%visibility %immutable %constant %override-specifier
     #'(lambda (vis imm con ove)
         `( :visibility ,vis  :immutable ,imm  :constant ,con  :override-specifier ,ove )))
   (%visibility %immutable %override-specifier %constant
     #'(lambda (vis imm ove con)
         `( :visibility ,vis  :immutable ,imm  :override-specifier ,ove  :constant ,con )))
   (%visibility %override-specifier %constant %immutable
     #'(lambda (vis ove con imm)
         `( :visibility ,vis  :override-specifier ,ove  :constant ,con  :immutable ,imm )))
   (%visibility %override-specifier %immutable %constant
     #'(lambda (vis ove imm con)
         `( :visibility ,vis  :override-specifier ,ove  :immutable ,imm  :constant ,con )))
   (%visibility %constant %override-specifier %immutable
     #'(lambda (vis con ove imm)
         `( :visibility ,vis  :constant ,con  :override-specifier ,ove  :immutable ,imm )))
   (%visibility %constant %immutable %override-specifier
     #'(lambda (vis con imm ove)
         `( :visibility ,vis  :constant ,con  :immutable ,imm  :override-specifier ,ove )))
   (%constant %immutable %visibility %override-specifier
     #'(lambda (con imm vis ove)
         `( :constant ,con  :immutable ,imm  :visibility ,vis  :override-specifier ,ove )))
   (%constant %immutable %override-specifier %visibility
     #'(lambda (con imm ove vis)
         `( :constant ,con  :immutable ,imm  :override-specifier ,ove  :visibility ,vis )))
   (%constant %override-specifier %visibility %immutable
     #'(lambda (con ove vis imm)
         `( :constant ,con  :override-specifier ,ove  :visibility ,vis  :immutable ,imm )))
   (%constant %override-specifier %immutable %visibility
     #'(lambda (con ove imm vis)
         `( :constant ,con  :override-specifier ,ove  :immutable ,imm  :visibility ,vis )))
   (%constant %override-specifier %immutable
     #'(lambda (con ove imm)
         `( :constant ,con  :override-specifier ,ove  :immutable ,imm )))
   (%constant %visibility %override-specifier %immutable
     #'(lambda (con vis ove imm)
         `( :constant ,con  :visibility ,vis  :override-specifier ,ove  :immutable ,imm )))
   (%visibility %override-specifier %immutable
     #'(lambda (vis ove imm)
         `( :visibility ,vis  :override-specifier ,ove  :immutable ,imm )))
   (%constant %immutable %override-specifier
     #'(lambda (con imm ove)
         `( :constant ,con  :immutable ,imm  :override-specifier ,ove )))
   (%constant %visibility %immutable %override-specifier
     #'(lambda (con vis imm ove)
         `( :constant ,con  :visibility ,vis  :immutable ,imm  :override-specifier ,ove )))
   (%visibility %immutable %override-specifier
     #'(lambda (vis imm ove)
         `( :visibility ,vis  :immutable ,imm  :override-specifier ,ove )))
   (%override-specifier %immutable %visibility %constant
     #'(lambda (ove imm vis con)
         `( :override-specifier ,ove  :immutable ,imm  :visibility ,vis  :constant ,con )))
   (%override-specifier %immutable %constant %visibility
     #'(lambda (ove imm con vis)
         `( :override-specifier ,ove  :immutable ,imm  :constant ,con  :visibility ,vis )))
   (%override-specifier %constant %visibility %immutable
     #'(lambda (ove con vis imm)
         `( :override-specifier ,ove  :constant ,con  :visibility ,vis  :immutable ,imm )))
   (%constant %visibility %immutable
     #'(lambda (con vis imm)
         `( :constant ,con  :visibility ,vis  :immutable ,imm )))
   (%override-specifier %immutable %visibility
     #'(lambda (ove imm vis)
         `( :override-specifier ,ove  :immutable ,imm  :visibility ,vis )))
   (%override-specifier %constant %immutable %visibility
     #'(lambda (ove con imm vis)
         `( :override-specifier ,ove  :constant ,con  :immutable ,imm  :visibility ,vis )))
   (%constant %immutable %visibility
     #'(lambda (con imm vis)
         `( :constant ,con  :immutable ,imm  :visibility ,vis )))
   (%override-specifier %constant %immutable
     #'(lambda (ove con imm)
         `( :override-specifier ,ove  :constant ,con  :immutable ,imm )))
   (%constant %immutable
     #'(lambda (con imm)
         `( :constant ,con  :immutable ,imm )))
   (%override-specifier %visibility %constant %immutable
     #'(lambda (ove vis con imm)
         `( :override-specifier ,ove  :visibility ,vis  :constant ,con  :immutable ,imm )))
   (%visibility %constant %immutable
     #'(lambda (vis con imm)
         `( :visibility ,vis  :constant ,con  :immutable ,imm )))
   (%override-specifier %immutable
     #'(lambda (ove imm)
         `( :override-specifier ,ove  :immutable ,imm )))
   (%immutable
     #'(lambda (imm)
         `( :immutable ,imm )))
   (%override-specifier %visibility %immutable
     #'(lambda (ove vis imm)
         `( :override-specifier ,ove  :visibility ,vis  :immutable ,imm )))
   (%visibility %immutable
     #'(lambda (vis imm)
         `( :visibility ,vis  :immutable ,imm )))
   (%override-specifier %immutable %constant
     #'(lambda (ove imm con)
         `( :override-specifier ,ove  :immutable ,imm  :constant ,con )))
   (%override-specifier %visibility %immutable %constant
     #'(lambda (ove vis imm con)
         `( :override-specifier ,ove  :visibility ,vis  :immutable ,imm  :constant ,con )))
   (%visibility %immutable %constant
     #'(lambda (vis imm con)
         `( :visibility ,vis  :immutable ,imm  :constant ,con )))
   (%immutable %override-specifier %visibility %constant
     #'(lambda (imm ove vis con)
         `( :immutable ,imm  :override-specifier ,ove  :visibility ,vis  :constant ,con )))
   (%override-specifier %visibility %constant
     #'(lambda (ove vis con)
         `( :override-specifier ,ove  :visibility ,vis  :constant ,con )))
   (%immutable %override-specifier %constant %visibility
     #'(lambda (imm ove con vis)
         `( :immutable ,imm  :override-specifier ,ove  :constant ,con  :visibility ,vis )))
   (%override-specifier %constant %visibility
     #'(lambda (ove con vis)
         `( :override-specifier ,ove  :constant ,con  :visibility ,vis )))
   (%immutable %constant %visibility %override-specifier
     #'(lambda (imm con vis ove)
         `( :immutable ,imm  :constant ,con  :visibility ,vis  :override-specifier ,ove )))
   (%constant %visibility %override-specifier
     #'(lambda (con vis ove)
         `( :constant ,con  :visibility ,vis  :override-specifier ,ove )))
   (%immutable %override-specifier %visibility
     #'(lambda (imm ove vis)
         `( :immutable ,imm  :override-specifier ,ove  :visibility ,vis )))
   (%override-specifier %visibility
     #'(lambda (ove vis)
         `( :override-specifier ,ove  :visibility ,vis )))
   (%immutable %constant %override-specifier %visibility
     #'(lambda (imm con ove vis)
         `( :immutable ,imm  :constant ,con  :override-specifier ,ove  :visibility ,vis )))
   (%constant %override-specifier %visibility
     #'(lambda (con ove vis)
         `( :constant ,con  :override-specifier ,ove  :visibility ,vis )))
   (%immutable %visibility
     #'(lambda (imm vis)
         `( :immutable ,imm  :visibility ,vis )))
   (%visibility
     #'(lambda (vis)
         `( :visibility ,vis )))
   (%immutable %constant %visibility
     #'(lambda (imm con vis)
         `( :immutable ,imm  :constant ,con  :visibility ,vis )))
   (%constant %visibility
     #'(lambda (con vis)
         `( :constant ,con  :visibility ,vis )))
   (%immutable %constant %override-specifier
     #'(lambda (imm con ove)
         `( :immutable ,imm  :constant ,con  :override-specifier ,ove )))
   (%constant %override-specifier
     #'(lambda (con ove)
         `( :constant ,con  :override-specifier ,ove )))
   (%immutable %visibility %constant %override-specifier
     #'(lambda (imm vis con ove)
         `( :immutable ,imm  :visibility ,vis  :constant ,con  :override-specifier ,ove )))
   (%visibility %constant %override-specifier
     #'(lambda (vis con ove)
         `( :visibility ,vis  :constant ,con  :override-specifier ,ove )))
   (%immutable %override-specifier
     #'(lambda (imm ove)
         `( :immutable ,imm  :override-specifier ,ove )))
   (%override-specifier
     #'(lambda (ove)
         `( :override-specifier ,ove )))
   (%immutable %visibility %override-specifier
     #'(lambda (imm vis ove)
         `( :immutable ,imm  :visibility ,vis  :override-specifier ,ove )))
   (%visibility %override-specifier
     #'(lambda (vis ove)
         `( :visibility ,vis  :override-specifier ,ove )))
   (%immutable %override-specifier %constant
     #'(lambda (imm ove con)
         `( :immutable ,imm  :override-specifier ,ove  :constant ,con )))
   (%override-specifier %constant
     #'(lambda (ove con)
         `( :override-specifier ,ove  :constant ,con )))
   (%immutable %visibility %override-specifier %constant
     #'(lambda (imm vis ove con)
         `( :immutable ,imm  :visibility ,vis  :override-specifier ,ove  :constant ,con )))
   (%visibility %override-specifier %constant
     #'(lambda (vis ove con)
         `( :visibility ,vis  :override-specifier ,ove  :constant ,con )))
   (%immutable %constant
     #'(lambda (imm con)
         `( :immutable ,imm  :constant ,con )))
   (%constant
     #'(lambda (con)
         `( :constant ,con )))
   (%immutable %visibility %constant
     #'(lambda (imm vis con)
         `( :immutable ,imm  :visibility ,vis  :constant ,con )))
   (%visibility %constant
     #'(lambda (vis con)
         `( :visibility ,vis  :constant ,con )))
   
   )
  (%stmnt
   (%block #'(lambda (blk) `(:stmnt-blk ,blk)))
  
   (%var-decl-stmnt #'(lambda (last) `(:var-decl-stmnt (,last))))
   (%var-decl-stmnt %stmnt #'(lambda (head rest)
                               `(:var-decl-stmnt ,(append (list head) (cadr rest)))))
  
   (%expr-stmnt #'(lambda (last) `(:expr-stmnt (,last))))
   (%expr-stmnt %stmnt #'(lambda (head rest)
                           `(:expr-stmnt ,(append (list head) (cadr rest)))))
  
   (%if-stmnt #'(lambda (last) `(:if-stmnt (,last))))
   (%if-stmnt %stmnt #'(lambda (head rest)
                         `(:if-stmnt ,(append (list head) (cadr rest)))))
  
   (%for-stmnt #'(lambda (last) `(:for-stmnt (,last))))
   (%for-stmnt %stmnt #'(lambda (head rest)
                          `(:for-stmnt ,(append (list head) (cadr rest)))))
  
   (%while-stmnt #'(lambda (last) `(:while-stmnt (,last))))
   (%while-stmnt %stmnt #'(lambda (head rest)
                            `(:while-stmnt ,(append (list head) (cadr rest)))))
  
   (%do-while-stmnt #'(lambda (last) `(:dowhile-stmnt (,last))))
   (%do-while-stmnt %stmnt #'(lambda (head rest)
                               `(:dowhile-stmnt ,(append (list head) (cadr rest)))))
  
   (%cont-stmnt #'(lambda (last) `(:cont-stmnt (,last))))
   (%cont-stmnt %stmnt #'(lambda (head rest)
                           `(:cont-stmnt ,(append (list head) (cadr rest)))))
  
   (%break-stmnt #'(lambda (last) `(:break-stmnt (,last))))
   (%break-stmnt %stmnt #'(lambda (head rest)
                            `(:break-stmnt ,(append (list head) (cadr rest)))))
  
   (%try-stmnt #'(lambda (last) `(:try-stmnt (,last))))
   (%try-stmnt %stmnt #'(lambda (head rest)
                          `(:try-stmnt ,(append (list head) (cadr rest)))))
  
   (%return-stmnt #'(lambda (last) `(:return-stmnt (,last))))
   (%return-stmnt %stmnt #'(lambda (head rest)
                             `(:return-stmnt ,(append (list head) (cadr rest)))))
  
   (%emit-stmnt #'(lambda (last) `(:emit-stmnt (,last))))
   (%emit-stmnt %stmnt #'(lambda (head rest)
                           `(:emit-stmnt ,(append (list head) (cadr rest)))))
  
   (%revert-stmnt #'(lambda (last) `(:revert-stmnt (,last))))
   (%revert-stmnt %stmnt #'(lambda (head rest)
                             `(:revert-stmnt ,(append (list head) (cadr rest)))))
  
   (%assembly-stmnt #'(lambda (last) `(:assembly-stmnt (,last))))
   (%assembly-stmnt %stmnt #'(lambda (head rest)
                               `(:assembly-stmnt ,(append (list head) (cadr rest)))))
  
   (%unchecked-block-contents #'(lambda (last) `(:unchecked-stmnt (,last))))
   (%unchecked-block-contents %stmnt #'(lambda (head rest)
                                         `(:unchecked-stmnt ,(append (list head) (cadr rest)))))
   )
  (%var-decl-stmnt
   (%var-decl |%;| #'(lambda (vd sc)
                       `(:var-decl ,vd)))
   (%var-decl |%=| %expr |%;| #'(lambda (vd eq ex sc)
                                  `(:var-decl ,vd :init ,ex)))
   (%var-decl-tuple |%=| %expr |%;| #'(lambda (vd eq ex sc)
                                        `(var-decl-tuple ,vd :init ,ex)))
   )
  (%var-decl
   (%type-name %identifier #'(lambda (vt vn)
                               `(:var-type ,vt :name ,vn)))
   (%type-name %error #'(lambda (vt vn)
                          `(:var-type ,vt )))
   (%type-name %data-location %identifier
               #'(lambda (vt dl vn)
                   `(:var-type ,vt :data-location ,dl :name ,vn)))
   )
  (%var-decl-tuple
   (|%(| %var-decl |%)| #'(lambda (lb vd rb)
                            `(:var-decl-tuple ,vd)))
   (|%(| %var-decl %one-more-var-decl |%)|
         #'(lambda (lb vd om rb)
             `(:var-decl-tuple ,vd :rest ,om)))
   )
  (%one-more-var-decl
   (|%,| %var-decl #'(lambda (cm vd)
                       `(one-more-var-decl ,vd)))
   (|%,| #'(lambda (cm)
             `(one-more-comma ,cm)))
   (|%,| %var-decl %one-more-var-decl
         #'(lambda (cm vd om)
             `(one-more-var-decl ,vd :rest ,om)))
   (|%,| %one-more-var-decl
         #'(lambda (cm om)
             `(one-more-comma ,cm :rest ,om)))
   )
  (%expr-stmnt
   (%expr |%;| #'(lambda (ex sc) `(:expr ,ex)))
   )
  (%if-stmnt
   (%if |%(| %expr |%)| %stmnt
        #'(lambda (ef lb ce rb th) `(if-stmnt ,ce :then ,th)))
   (%if |%(| %expr |%)| %stmnt %else %stmnt
        #'(lambda (ef lb ce rb th el es)
            `(if-stmnt ,ce :then ,th :else ,es)))
   )
  (%for-stmnt
   (%for |%(|            |%;|  |%;|                |%)| %stmnt
         #'(lambda (fr lb c1 c2 rb bd)
             `(:for :var nil :cond nil :step nil :body ,bd)))
   (%for |%(| %var-decl-stmnt  |%;|                |%)| %stmnt
         #'(lambda (fr lb c1 c2 rb bd)
             `(:for :var ,c1 :cond nil :step nil :body ,bd)))
   (%for |%(|     %expr-stmnt  |%;|                |%)| %stmnt
         #'(lambda (fr lb c1 c2 rb bd)
             `(:for :var ,c1 :cond nil :step nil :body ,bd)))
   ;;
   (%for |%(|            |%;|  %expr-stmnt         |%)| %stmnt
         #'(lambda (fr lb c1 c2 rb bd)
             `(:for :var nil :cond ,c2 :step nil :body ,bd)))
   (%for |%(| %var-decl-stmnt  %expr-stmnt         |%)| %stmnt
         #'(lambda (fr lb c1 c2 rb bd)
             `(:for :var ,c1 :cond ,c2 :step nil :body ,bd)))
   (%for |%(|     %expr-stmnt  %expr-stmnt         |%)| %stmnt
         #'(lambda (fr lb c1 c2 rb bd)
             `(:for :var ,c1 :cond ,c2 :step nil :body ,bd)))
   ;;
   (%for |%(|            |%;|  |%;|         %expr  |%)| %stmnt
         #'(lambda (fr lb c1 c2 c3 rb bd)
             `(:for :var nil :cond nil :step ,c3 :body ,bd)))
   (%for |%(| %var-decl-stmnt  |%;|         %expr  |%)| %stmnt
         #'(lambda (fr lb c1 c2 c3 rb bd)
             `(:for :var ,c1 :cond ,c2 :step ,c3 :body ,bd)))
   (%for |%(|     %expr-stmnt  |%;|         %expr  |%)| %stmnt
         #'(lambda (fr lb c1 c2 c3 rb bd)
             `(:for :var ,c1 :cond ,c2 :step ,c3 :body ,bd)))
   ;;
   (%for |%(|            |%;|  %expr-stmnt  %expr  |%)| %stmnt
         #'(lambda (fr lb c1 c2 c3 rb bd)
             `(:for :var nil :cond ,c2 :step ,c3 :body ,bd)))
   (%for |%(| %var-decl-stmnt  %expr-stmnt  %expr  |%)| %stmnt
         #'(lambda (fr lb c1 c2 c3 rb bd)
             `(:for :var ,c1 :cond ,c2 :step ,c3 :body ,bd)))
   (%for |%(|     %expr-stmnt  %expr-stmnt  %expr  |%)| %stmnt
         #'(lambda (fr lb c1 c2 c3 rb bd)
             `(:for :var ,c1 :cond ,c2 :step ,c3 :body ,bd)))
   )
  (%while-stmnt
   (%while |%(| %expr |%)| %stmnt
         #'(lambda (wh lb ex rb bd)
             `(:while ,ex :body ,bd)))
   )
  (%do-while-stmnt
   (%do %stmnt %while |%(| %expr |%)| |%;|
         #'(lambda (dd bd wh lb ex rb sc)
             `(:dowhile ,ex :body ,bd)))
   )
  (%cont-stmnt
   (%yul-continue |%;|
         #'(lambda (co sc)
             `(:cont nil)))
   )
  (%break-stmnt
   (%yul-break |%;|
         #'(lambda (br sc)
             `(:break nil)))
   )
  (%try-stmnt
   (%try %expr %block %catch-clause-contents
         #'(lambda (tr ex bl cc)
             `(:try ,ex :blk ,bl :catch ,cc)))
   (%try %expr %returns |%(| %param-list |%)| %block %catch-clause-contents
         #'(lambda (tr ex rt lb pl rb bl cc)
             `(:try ,ex :ret ,pl :blk ,bl :catch ,cc)))
   )
  (%catch-clause-contents
   (%catch-clause #'(lambda (last) `(:catch (,last))))
   (%catch-clause %catch-clause-contents
                  #'(lambda (head rest) `(:catch ,(append (list head)
                                                          (getf rest :catch)))))
   )
  (%catch-clause
   (%catch %block #'(lambda (ca bl) `(:catch nil :blk ,bl)))
   (%catch |%(| %param-list |%)| %block
           #'(lambda (ca lb pl rb bl) `(:catch nil :par-list ,pl :blk ,bl)))
   (%catch %identifier |%(| %param-list |%)| %block
           #'(lambda (ca id lb pl rb bl) `(:catch ,id :par-list ,pl :blk ,bl)))
   )
  (%return-stmnt
   (%return |%;|
         #'(lambda (rt sc)
             `(:return nil)))
   (%return %expr |%;|
            #'(lambda (rt ex sc)
                `(:return ,ex)))
   (%return %addr |%;|
            #'(lambda (rt ex sc)
                `(:return ,ex)))
   (%return %hex-num |%;|
            #'(lambda (rt ex sc)
                `(:return ,ex)))
   )
  (%emit-stmnt
   (%emit %expr %call-arg-list |%;|
            #'(lambda (et ex al sc)
                `(:emit ,ex, :args ,al)))
   )
  (%revert-stmnt
   (%revert %expr %call-arg-list |%;|
            #'(lambda (re ex al sc)
                `(:revert ,ex, :args ,al)))
   (%revert %call-arg-list |%;|
            #'(lambda (re al sc)
                `(:revert nil, :args ,al)))
   )
  (%assembly-stmnt
   (%assembly |%{| |%}| #'(lambda (as lb rb) `(:asm-empty nil)))
   (%assembly |%{| %yul-stmnt-contents |%}| #'(lambda (as lb yu rb) `(:asm ,yu)))
   (%assembly %evmasm |%{|  |%}| #'(lambda (as em lb rb) `(:asm-empty nil)))
   (%assembly %evmasm |%{| %yul-stmnt-contents |%}| #'(lambda (as em lb yu rb) `(:asm nil)))
   )
  (%yul-stmnt-contents
   (%yul-stmnt #'(lambda (last) `(:yul-src (,last))))
   (%yul-stmnt %yul-stmnt-contents
                        #'(lambda (head rest)
                            `(:yul-src ,(append (list head) (cadr rest))))))
  (%yul-stmnt
   (%yul-block #'(lambda (yb) `(:y-block ,yb)))
   (%yul-var-decl #'(lambda (yvs) `(:y-var ,yvs)))
   (%yul-assignmnt #'(lambda (ya) `(:y-assignmnt ,ya)))
   (%yul-func-call #'(lambda (yfc) `(:y-f-call ,yfc)))
   (%yul-if-stmnt #'(lambda (yi) `(:y-if-stmnt ,yi)))
   (%yul-for #'(lambda (yf) `(:y-for ,yf)))
   (%yul-switch #'(lambda (ys) `(:y-switch ,ys)))
   (%yul-break #'(lambda (yb) `(:y-break ,yb)))
   (%yul-leave #'(lambda (yl) `(:y-leave ,yl)))
   (%yul-continue #'(lambda (yc) `(:y-continue ,yc)))
   (%yul-func-def #'(lambda (yfd) `(:y-func-def ,yfd)))
   )
  (%yul-break
   (%break #'(lambda (yb) `(:y-break nil))))
  
  (%yul-leave
   (%leave #'(lambda (yl) `(:y-leave nil))))
  
  (%yul-continue
   (%continue #'(lambda (yc) `(:y-continue nil))))
  (%yul-block
   ( |%{| |%}| #'(lambda (lb rb) `(:empty-y-block nil)))
   ( |%{| %yul-stmnt-contents |%}| #'(lambda ( lbr y-stmnt rbr)
                                       `(:stmnt ,y-stmnt)))
   )
  (%yul-var-decl
   (%let %identifier #'(lambda (var id)
                             `(:v-id ,id)))
  
   ;; (%let %identifier |%:=| %yul-expr
   ;;       #'(lambda (var id assgn expr)
   ;;           `(:v-id ,id :y-expr ,expr)))
  
   (%let %identifier |%:| |%=| %yul-expr
         #'(lambda (var id colon assgn expr)
             `(:v-id ,id :y-expr ,expr)))
  
   (%let %yul-var-ident
         #'(lambda (var id) `(:v-id ,id)))
  
   ;; (%let %yul-var-ident |%:=| %yul-func-call
   ;;       #'(lambda (var id assgn fcall)
   ;;           `(:v-id ,id :y-fcall ,fcall)))
  
   (%let %yul-var-ident |%:| |%=| %yul-func-call
         #'(lambda (var id colon assgn fcall)
             `(:v-id ,id :y-fcall ,fcall)))
  
   )
  
  ;; (%yul-var-ident
  ;;  (%yul-identifier #'(lambda (id) `(:y-id ,id)))
  ;;  (%yul-identifier |%,| %yul-var-ident #'(lambda (id1 id2)
  ;;                                           `(:y-id-head ,id1 :y-id-rest ,id2))))
  
  (%yul-var-ident
   (%identifier #'(lambda (id) `(:y-id ,id)))
   (%identifier |%,| %yul-var-ident #'(lambda (id1 id2)
                                            `(:y-id-head ,id1 :y-id-rest ,id2))))
  (%yul-expr
   (%yul-path #'(lambda (yfc) `(:y-fcall ,yfc)))
   (%yul-func-call #'(lambda (yfc) `(:y-fcall ,yfc)))
   (%yul-lit #'(lambda (yl) `(:y-lit ,yl)))
   (%addr #'(lambda (yld) `(:y-addrt ,yld)))
   )
  (%yul-path
   (%identifier #'(lambda (yid) `(:y-path-head ,yid)))
   (%identifier |%.| %yul-path #'(lambda (yid1 pt yid2)
                                       `(:y-id-head ,yid1 :y-id-rest ,yid2))))
  (%yul-func-call
   (%identifier |%(| |%)|
                #'(lambda (yid lsc rsc) `(:y-id ,yid)))
   (%yul-envbuiltin |%(| |%)|
                    #'(lambda (yenv lsc rsc) `(:y-env ,yenv)))
   (%identifier |%(| %yul-func-call-body |%)|
                #'(lambda (yid lsc fcbody rsc) `(:y-id ,yid :y-fbody ,fcbody)))
   (%yul-envbuiltin |%(| %yul-func-call-body |%)|
                    #'(lambda (yenv lsc  fcbody rsc) `(:y-env ,yenv :y-fbody ,fcbody))))
  
  
   (%yul-func-call-body
    (%yul-expr #'(lambda (exp) `(:y-exp ,exp)))
    (%yul-expr |%,| %yul-func-call-body
               #'(lambda (exp1 cm exp2)
                   `(:y-fcallb-head ,exp1 :y-fcallb-rest ,exp2))))
  
  
  (%yul-envbuiltin
   (%return #'(lambda (cmd) `(:asm-cmd ,cmd )))
   (%revert #'(lambda (cmd) `(:asm-cmd ,cmd )))
   (%identifier #'(lambda (cmd) `(:asm-cmd ,cmd ))))
  
  (%yul-lit
   (%dec-num #'(lambda (lt) `(:num-lit ,lt)))
   (%str-lit #'(lambda (lt) `(:str-lit ,lt)))
   ;; (%yul-str-lit #'(lambda (lt) `(:str-lit ,lt)))
   (%bool-lit #'(lambda (lt) `(:bool-lit ,lt)))
   ;; (%yul-bool-lit #'(lambda (lt) `(:bool-lit ,lt)))
   ;; (%yul-hex-num #'(lambda (lt) `(:hexstr-lit ,lt)))
   (%hex-num #'(lambda (lt) `(:hexstr-lit ,lt)))
   (%hex-str #'(lambda (lt) `(:hexstr-lit ,lt)))
   )
  
  ;; (%yul-dec-lit  #'(lambda (lt)
  ;;                    `(:yul-dec-lit ,lt)))
  ;; (%yul-str-lit
  ;;  (%empty-str #'(lambda (lt) `(:str-lit ,lt)))
  ;;  (%non-empty-str #'(lambda (lt) `(:str-lit ,lt)))
  ;;  )
  ;; (%yul-hex-num
  ;;  #'(lambda (hn) `(:yul-hex-num ,hn)))
  (%yul-assignmnt
   (%yul-assignmnt-left-side |%:| |%=| %yul-expr
                             #'(lambda (lsd colon assign exp)
                                 `(:lsd-asgmnt ,lsd :assign-exp ,exp)))
   ;; (%yul-assignmnt-left-side |%:=| %yul-expr
   ;;                           #'(lambda (lsd assign exp)
   ;;                               `(:lsd-asgmnt ,lsd :assign-exp ,exp)))
   )
  
  (%yul-assignmnt-left-side
   (%yul-path  #'(lambda (lsd) `(:lsd-head ,lsd)))
   (%yul-path |%,| %yul-assignmnt-left-side
              #'(lambda (lsd1 comma lsd2)
                  `(:lsd-head ,lsd1 :lsd-rest ,lsd2))))
  
  (%yul-if-stmnt
   (%if %yul-expr %yul-stmnt
        #'(lambda (condit exp block)
            `(:y-if-expr ,exp :y-if-block ,block))))
  (%yul-for
   (%for %yul-block %yul-expr %yul-block %yul-block
         #'(lambda (op block1 exp block2 block3)
             `(:for-block1 ,block1 :for-exp ,exp
               :for-block1 ,block2 :for-block1 ,block3))))
  
  (%yul-switch
   (%switch %yul-expr %switch-body %switch-defalut
            #'(lambda (op swexpr swbody swdeflt)
                `(:y-swexpr ,swexpr :y-swbody ,swbody :y-swdeflt ,swdeflt)))
  
   (%switch %yul-expr %switch-body
            #'(lambda (op swexpr swbody)
                `(:y-swexpr ,swexpr :y-swbody ,swbody)))
  
   (%switch %yul-expr %switch-defalut
            #'(lambda (op swexpr swdeflt)
                `(:y-swexpr ,swexpr :y-swdeflt ,swdeflt))))
  
  
  
  (%switch-defalut
   (%default %yul-block
            #'(lambda (op swblock)
                `(:y-swblock ,swblock))))
  
  
  (%switch-body
   (%case %yul-lit %yul-block
             #'(lambda (op swlit swblock)
                 `(:y-swlit ,swlit :swbody-swblock-head ,swblock)))
  
   (%case %yul-lit %yul-block %switch-body
          #'(lambda (op swlit swblock swbody-rec)
              `(:y-swlit ,swlit :swbody-swblock-head ,swblock
                         :swbody-swblock-last ,swbody-rec))))
  (%yul-func-def
   (%function %identifier |%(| |%)| |%->|
              %multi-ident-path %yul-block
        #'(lambda (func y-id lsc rsc thfr m-id fbody)
            `(:func-y-id ,y-id :multi-id ,m-id :y-fbody ,fbody)))
  
   (%function %identifier |%(| %multi-ident-path |%)| |%->|
              %multi-ident-path %yul-block
              #'(lambda (func y-id lsc m-id1 rsc thfr m-id2 fbody)
                  `(:func-y-id ,y-id :multi-id1 ,m-id1
                    :multi-id2 ,m-id2 :y-fbody ,fbody))))
  (%expr
   ;; index access
   (%expr |%[| |%]| #'(lambda (e1 lb rb) `(:expr-idx-empty ,e1)))
   (%expr |%[| %expr |%]| #'(lambda (e1 lb e2 rb) `(:expr-idx ,e1 :idx ,e2)))
   ;; index range
   (%expr |%[| |%:| |%]|
          #'(lambda (e1 lb c rb) `(:expr-idx-range ,e1 :from nil :to nil)))
   (%expr |%[| |%:| %expr |%]|
          #'(lambda (e1 lb c e2 rb) `(:expr-idx-range ,e1 :from nil, :to ,e2)))
   (%expr |%[| %expr |%:| |%]|
          #'(lambda (e1 lb e2 c rb) `(:expr-idx-range ,e1 :from ,e2 :to nil)))
   (%expr |%[| %expr |%:| %expr |%]|
          #'(lambda (e1 lb e2 c e3 rb) `(:expr-idx-range ,e1 :from ,e2 :to ,e3)))
   ;; member access
   (%expr |%.| %identifier #'(lambda (ex dt id) `(:expr-identifier ,ex :id ,id)))
   (%expr |%.| %addr       #'(lambda (ex dt ad) `(:expr-addr       ,ex :add ,ad)))
   ;; idex
   (%expr |%{| |%}| #'(lambda (ex lb rb) `(:expr-idex-empty nil)))
   (%expr |%{| %idex-contents |%}| #'(lambda (ex lb idex rb) `(:expr-idex ,idex)))
   ;; expr with call-arg-list
   (%expr %call-arg-list #'(lambda (ex cal) `(:expr-call ,ex :arg-lst ,cal)))
   ;; payable
   (%payable %call-arg-list #'(lambda (ex cal) `(:expr-call ,ex :arg-lst ,cal)))
   ;; type
   (%type |%(| %type-name |%)| #'(lambda (tl lb tn rb) `(:expr-type ,tn)))
   ;; math
   (|%++| %expr #'(lambda (op ex) `(:expr-pre-inc ,ex)))
   (|%--| %expr #'(lambda (op ex) `(:expr-pre-dec ,ex)))
   (|%!|  %expr #'(lambda (op ex) `(:expr-not ,ex)))
   (|%~|  %expr #'(lambda (op ex) `(:expr-tilda ,ex)))
   (%delete  %expr #'(lambda (op ex) `(:expr-delete ,ex)))
   (|%-|  %expr #'(lambda (op ex) `(:expr-unary-minus ,ex)))
   (%expr |%++| #'(lambda (ex op) `(:expr-post-inc ,ex)))
   (%expr |%--| #'(lambda (ex op) `(:expr-post-dec ,ex)))
   (%expr |%**| %expr #'(lambda (e1 op e2) `(:expr-power ,e1 :arg ,e2)))
   (%expr |%*|  %expr #'(lambda (e1 op e2) `(:expr-mul   ,e1 :arg ,e2)))
   (%expr |%/|  %expr #'(lambda (e1 op e2) `(:expr-div   ,e1 :arg ,e2)))
   (%expr |%%|  %expr #'(lambda (e1 op e2) `(:expr-mod   ,e1 :arg ,e2)))
   (%expr |%+|  %expr #'(lambda (e1 op e2) `(:expr-add   ,e1 :arg ,e2)))
   (%expr |%-|  %expr #'(lambda (e1 op e2) `(:expr-sub   ,e1 :arg ,e2)))
   (%expr |%>>>|  %expr #'(lambda (e1 op e2) `(:expr->>>   ,e1 :arg ,e2)))
   (%expr |%>>|  %expr #'(lambda (e1 op e2) `(:expr->>   ,e1 :arg ,e2)))
   (%expr |%<<|  %expr #'(lambda (e1 op e2) `(:expr-<<   ,e1 :arg ,e2)))
   (%expr |%&|  %expr #'(lambda (e1 op e2) `(:expr-&   ,e1 :arg ,e2)))
   (%expr |%^|  %expr #'(lambda (e1 op e2) `(:expr-^   ,e1 :arg ,e2)))
   (%expr |%pipe|  %expr #'(lambda (e1 op e2) `(:expr-pipe   ,e1 :arg ,e2)))
   (%expr |%<|  %expr #'(lambda (e1 op e2) `(:expr-less      ,e1 :arg ,e2)))
   (%expr |%>|  %expr #'(lambda (e1 op e2) `(:expr-more      ,e1 :arg ,e2)))
   (%expr |%<=| %expr #'(lambda (e1 op e2) `(:expr-less-eql  ,e1 :arg ,e2)))
   (%expr |%>=| %expr #'(lambda (e1 op e2) `(:expr-more-eql  ,e1 :arg ,e2)))
   (%expr |%==| %expr #'(lambda (e1 op e2) `(:expr-eql       ,e1 :arg ,e2)))
   (%expr |%!=| %expr #'(lambda (e1 op e2) `(:expr-not-eql   ,e1 :arg ,e2)))
   (%expr |%&&| %expr #'(lambda (e1 op e2) `(:expr-and       ,e1 :arg ,e2)))
   (%expr |%pipepipe| %expr #'(lambda (e1 op e2) `(:expr-or  ,e1 :arg ,e2)))
   (%expr |%?| %expr #'(lambda (e1 op e2) `(:expr-question   ,e1 :arg ,e2)))
   (%expr |%=|     %expr #'(lambda (e1 op e2) `(:expr-eq   ,e1 :arg ,e2)))
   (%expr |%pipe=| %expr #'(lambda (e1 op e2) `(:expr-or-eq   ,e1 :arg ,e2)))
   (%expr |%^=|    %expr #'(lambda (e1 op e2) `(:expr-^-eq   ,e1 :arg ,e2)))
   (%expr |%&=|    %expr #'(lambda (e1 op e2) `(:expr-&-eq   ,e1 :arg ,e2)))
   (%expr |%<<=|   %expr #'(lambda (e1 op e2) `(:expr-less-less-eq   ,e1 :arg ,e2)))
   (%expr |%>>=|   %expr #'(lambda (e1 op e2) `(:expr-more-more-eq   ,e1 :arg ,e2)))
   (%expr |%>>>=|  %expr #'(lambda (e1 op e2) `(:expr-more-more-more-eq   ,e1 :arg ,e2)))
   (%expr |%+=|    %expr #'(lambda (e1 op e2) `(:expr-plus-eq   ,e1 :arg ,e2)))
   (%expr |%-=|    %expr #'(lambda (e1 op e2) `(:expr-minus-eq   ,e1 :arg ,e2)))
   (%expr |%*=|    %expr #'(lambda (e1 op e2) `(:expr-mul-eq   ,e1 :arg ,e2)))
   (%expr |%/=|    %expr #'(lambda (e1 op e2) `(:expr-div-eq   ,e1 :arg ,e2)))
   (%expr |%:|     %expr #'(lambda (e1 op e2) `(:expr-div-eq   ,e1 :arg ,e2)))
   (%expr |%%=|    %expr #'(lambda (e1 op e2) `(:expr-mod-eq   ,e1 :arg ,e2)))
   (%expr %identifier #'(lambda (e1 id) `(:expr1 , e1 :expr-ident ,id)))
   (%error #'(lambda (err) `(:expr-err nil)))
   (%new %type-name #'(lambda (new tn) `(:new ,tn)))
   (%tuple-expression #'(lambda (te) `(:expr-tuple ,te)))
   (%inline-array-expr #'(lambda (ia) `(:expr-inline-array ,ia)))
   (%lit #'(lambda (lit) `(:expr-lit ,lit)))
   (%addr #'(lambda (addr) `(:expr-addr ,addr)))
   (%type-name #'(lambda (tn) `(:expr-tn ,tn)))
   (%elt-type-name #'(lambda (etn) `(:elt-type-name ,etn)))
   )
  (%idex-contents ;; TODO: Кажется это multi-ident-path
   (%idex #'(lambda (last) `(:idex (,last))))
   (%idex |%,| %idex-contents #'(lambda (head co rest)
                                  `(:idex ,(append (list head)
                                                   (getf rest :idex)))))
   )
  (%idex
   (%identifier |%:| %expr #'(lambda (id cm ex) `(:idex-id ,id :expr ,ex)))
   )
  (%tuple-expression
   (|%(| %expr-comma-list |%)| #'(lambda (lb ecl rb) `(:tuple ,ecl)))
   )
  (%inline-array-expr
   (|%[| %expr-comma-list |%]| #'(lambda (lb ex rb) `(:inline-array-expr ,ex)))
   (|%[| |%]| #'(lambda (lb rb) `(:inline-array-expr nil)))
   (|%[| |%:| %expr |%]| #'(lambda (lb colon ex rb) `(:inline-array-expr ,ex)))
   )
  (%lit
   (%num-lit #'(lambda (lt) `(:num-lit ,lt)))
   (%str-lit #'(lambda (lt) `(:str-lit ,lt)))
   (%bool-lit #'(lambda (lt) `(:bool-lit ,lt)))
   ;; (%hexstr-litl #'(lambda (lt) `(:hexstr-lit ,lt)))
   ;; (%unicodestr-lit #'(lambda (lt) `(:unicodestr-lit ,lt)))
   )
  (%str-lit
   (%empty-str #'(lambda (lt) `(:str-lit ,lt)))
   (%non-empty-str #'(lambda (lt) `(:str-lit ,lt)))
   )
  (%empty-str
   (%single-quoted-printable-empty #'(lambda (es) `(:emp-str ,es)))
   (%double-quoted-printable-empty #'(lambda (es) `(:emp-str ,es)))
   )
  (%non-empty-str
   (%single-quoted-printable #'(lambda (es) `(:str ,es)))
   (%double-quoted-printable #'(lambda (es) `(:str ,es)))
   )
  (%num-lit
   (%dec-num #'(lambda (lt) `(:num-lit ,lt)))
   ;; (%dec-num %num-unit #'(lambda (lt nu)
   ;;                         `(:dec-num ,lt :num-unit ,nu)))
   (%hex-num #'(lambda (lt) `(:hexnum ,lt)))
   ;; (%hex-num %num-unit #'(lambda (lt nu)
   ;;                         `(:hex-num ,lt :num-unit ,nu)))
   )
  (%dec-num
   (%dec_num_v1 #'(lambda (dn) `(:dec-num ,dn)))
   ;; (%dec-num %num-unit #'(lambda (lt nu)
   ;;                         `(:dec-num ,lt :num-unit ,nu)))
   ;; (%hex-num #'(lambda (lt) `(:hex-number ,lt)))
   ;; (%hex-num %num-unit #'(lambda (lt nu)
   ;;                         `(:hex-num ,lt :num-unit ,nu)))
   )
  (%hex-num
   (%hex_num_v1 #'(lambda (hn) `(:hex-num ,hn)))
   (%hex_num_v2 #'(lambda (hn) `(:hex-num ,hn)))
   (%hex_num_v3 #'(lambda (hn) `(:hex-num ,hn)))
   )

  (%term
   %import %as
   %constant
   %pragma
   %library
   %using
   %event %anonymous %indexed
   %enum
   %contract %abstract %identifier %is |%.| |%,| |%{| |%}|
   ;; |%:=|
   |%:| |%(| |%)|
   %function |%;|
   %data-location
   %returns
   %visibility
   %pure
   %view
   %payable
   %virtual
   %override
   |%=>| %mapping
   %unsigned-integer-type %signed-integer-type %fixed-bytes %string
   %bytes %fixed %ufixed %bool %address %payable
   %unchecked
   %constructor
   %interface
   %modifier
   %fallback
   %receive
   %struct
   %error
   %immutable
   %if %else
   %for
   %while
   %do
   %continue
   %break
   %try
   %catch
   %return
   %emit
   %revert
   %assembly %evmasm
   %leave
   %let
   ;; %yul-identifier
   ;; %asm-command
   ;; %yul-dec-lit
   ;; %yul-bool-lit
   ;; %yul-hex-num
   %switch %case %default
   |%->|
   |%[| |%]| |.| %addr %type |%++| |%--| |%!| |%~| %delete |%-| |%*| |%**| |%/| |%%| |%+| |%-| |%>>>| |%>>| |%<<| |%&| |%^| |%pipe| |%<| |%>| |%<=| |%>=| |%==| |%=| |%!=| |%&&| |%pipepipe| |%?| |%pipe=| |%^=| |%&=| |%<<=| |%>>=| |%>>>=| |%+=| |%-=| |%*=| |%/=| |%%=| %new
   ;; %built-in-func
   %single-quoted-printable-empty %double-quoted-printable-empty
   %single-quoted-printable %double-quoted-printable
   %dec_num_v1
   %hex_num_v1
   %hex_num_v2
   %hex_num_v3
   %bool-lit
   ))
