; --------------------------------------
; Programa: Damas Havaianas
; --------------------------------------
;
;
; Criado por: Ricardo Leitão (69632)
;             Diogo Andrade  (70031)
;             Fábio Almeida  (70227)
;
; Grupo Nº:   27
;
; Data de criação: 04 de Novembro de 2010
;

; ----------------------------------------------
; [=] Definição Tipo:  Listas Simplificada
; ----------------------------------------------

; ---------------------------
; [»] Operações Básicas
; ---------------------------

; ========== Construtores ==========

(define (nova-lista)
  ())

(define (insere el lst)
      (cons el lst))
      
; ========== Selectores ==========

(define (primeiro lst)
  (if (null? lst)
      (error "primeiro: a lista não tem elementos")
      (car lst)))

(define (resto lst)
  (if (null? lst)
      (error "resto: a lista não tem elementos")
      (cdr lst)))

; ========== Reconhecedores ==========

(define (lista? x)
  (cond ((null? x) #t)
        ((pair? x) (lista? (cdr x)))
        (else #f)))

(define (lista-vazia? lst)
  (null? lst))

; ========== Testes ==========

(define (listas=? lst1 lst2)
  (cond ((null? lst1) (null? lst2))
        ((null? lst2) #f)
        ((elem=? (car lst1) (car lst2))
         (listas=? (cdr lst1) (cdr lst2)))
        (else #f)))

; =========== Operações de Alto Nível ===============

(define (comprimento-lista lst)
  (if (lista-vazia? lst)
      0
      (+ 1 (comprimento (resto lst)))))
(define (elementos-atomicos lst)
  (cond ((lista-vazia? lst) 0)
        ((lista? (primeiro lst))
         (+ (elementos-atomicos (primeiro lst))
            (elementos-atomicos (resto lst))))
        (else (+ 1 (elementos-atomicos (resto lst))))))
(define (junta lst1 lst2)
  (if (lista-vazia? lst1)
      lst2
      (insere (primeiro lst1) (junta (resto lst1) lst2))))
(define (inverte lst)
  (define (inverte-aux lst1 lst2)
    (if (lista-vazia? lst1)
        lst2
        (inverte-aux (resto lst1)
                     (insere (primeiro lst1) lst2))))
  (inverte-aux lst (nova-lista)))





; ---------------------------------
; [=] Definição Tipo: Posição
; ---------------------------------

; ----------------------------
; [»] Operações Básicas
; ----------------------------

; ========== Construtores ==========

(define (faz-pos l c) 
  (if (and (< l 8) (< c 8) (> l -1) (> c -1))
      (if (and (integer? l) (integer? c))
          (cons l c)
          (error "faz-pos: posição não é válida"))
      (error "faz-pos: posição não é válida")))

; ========== Selectores ==========

(define (linha-pos p)
  (car p))

(define (coluna-pos p)
  (cdr p))

; ========== Reconhecedor ==========

(define (pos? arg)
  (and (pair? arg)
       (integer? (car arg))
       (integer? (cdr arg))
       (and (< (car arg) 8) (> (car arg) -1))
       (and (< (cdr arg) 8) (> (cdr arg) -1))))

; ========== Teste ==========

(define (pos=? p1 p2)
  (and (= (car p1) (car p2))
       (= (cdr p1) (cdr p2))))

; --------------------------------
; [»]Operações de Alto Nível
; --------------------------------

(define (distancia p1 p2)
  (sqrt (+ (* (- (linha-pos p1) (linha-pos p2))
              (- (linha-pos p1) (linha-pos p2)))
           (* (- (coluna-pos p1) (coluna-pos p2))
              (- (coluna-pos p1) (coluna-pos p2))))))

(define (mesma-direccao? p1 p2)
  (or (= (linha-pos p1) (linha-pos p2))
      (= (coluna-pos p1) (coluna-pos p2))))

(define (adjacentes? p1 p2)
  (and (mesma-direccao? p1 p2)
       (= (distancia p1 p2) 1)))

(define (adjacentes p)
  (define (faz-pos-aux l c)
  ; em auxilio dos casos em que as posições contêm a coluna ou linha 7.
          (cons l c))
  (define (adj-aux lista1 lista2)
    (cond ((lista-vazia? lista2) lista1)
          ((pos? (primeiro lista2))
           (adj-aux (insere (primeiro lista2) lista1) (resto lista2)))
          (else (adj-aux lista1 (resto lista2)))))
  (if (pos? p)
      (adj-aux (nova-lista) (list (faz-pos-aux (+ (linha-pos p) 1) (coluna-pos p))
                                  (faz-pos-aux (- (linha-pos p) 1) (coluna-pos p))
                                  (faz-pos-aux (linha-pos p) (+ (coluna-pos p) 1))
                                  (faz-pos-aux (linha-pos p) (- (coluna-pos p) 1))))
      (error "adjacentes: posição fornecida não é válida")))


; ----------------------------------
; [=] Definição Tipo: Conteúdo
; ----------------------------------


; --------------------------------
; [=] Definição Tipo: Jogada
; --------------------------------

; ---------------------------
; [»] Operações Básicas
; ---------------------------

; ========== Construtor ==========

(define (faz-jogada p1 p2)
  (if (and (pos? p1) (pos? p2))
      (if (and (mesma-direccao? p1 p2))
          (cons p1 p2)
          (error "faz-jogada: esta jogada não é válida"))
      (error "faz-jogada: p1 ou p2 nao são posições")))

; ========== Selectores ==========

(define (inicio-jogada j)
  (car j))

(define (fim-jogada j)
  (cdr j))

; ========== Reconhecedores ==========

(define (jogada? arg)
  (and (pair? arg)
       (pos? (car arg))
       (pos? (cdr arg))))

(define (jogada-nula? j)
  (and (= (linha-pos (car j))(linha-pos (cdr j)))
      (= (coluna-pos (car j))(coluna-pos (cdr j)))))
   
     
; --------------------------------------------
; [=] Definição Tipo: Posições de Jogada
; --------------------------------------------

; ---------------------------
; [»] Operações Básicas
; ---------------------------

; ========== Construtores ==========

(define (faz-posicoes-de-jogada pc pl)
  (cond ((not (lista? pc)) (error "faz-posicoes-de-jogada: o primeiro argumento não é uma lista"))
        ((not (lista? pl)) (error "faz-posicoes-de-jogada: o segundo argumento não é uma lista"))
        (else (cons pc pl))))

; ========== Selectores ==========

(define (pecas-capturadas pj)
  (car pj))

(define (posicoes-livres pj)
  (cdr pj))

; ---------------------------------
; [»] Operações de Alto Nível
; ---------------------------------

(define (posicoes-de-jogada j)
  (define (faz-posicoes-de-jogada-aux-linha-cresc j lista-pc lista-pl)
    ; cria as posições de uma jogada que decorre ao longo duma linha de forma crescente, ou seja, por exemplo a jogada ((1 . 1) (1 . 7))
    (if (pos=? (fim-jogada j) 
               (primeiro lista-pl))
        (faz-posicoes-de-jogada (inverte lista-pc) (inverte lista-pl))
        (faz-posicoes-de-jogada-aux-linha-cresc j 
                                                (insere (faz-pos (linha-pos (primeiro lista-pc))
                                                                 (+ 2 (coluna-pos (primeiro lista-pc)))) 
                                                        lista-pc)
                                                (insere (faz-pos (linha-pos (primeiro lista-pl))
                                                                 (+ 2 (coluna-pos (primeiro lista-pl))))
                                                        lista-pl))))
  (define (faz-posicoes-de-jogada-aux-linha-decresc j lista-pc lista-pl)
    ; cria as posições de uma jogada que decorre ao longo duma linha de forma decrescente, ou seja, por exemplo a jogada ((1 . 7) (1 . 1))
    (if (pos=? (fim-jogada j) 
               (primeiro lista-pl))
        (faz-posicoes-de-jogada (inverte lista-pc) (inverte lista-pl))
        (faz-posicoes-de-jogada-aux-linha-decresc j 
                                                  (insere (faz-pos (linha-pos (primeiro lista-pc))
                                                                   (- (coluna-pos (primeiro lista-pc)) 2)) 
                                                          lista-pc)
                                                  (insere (faz-pos (linha-pos (primeiro lista-pl))
                                                                   (- (coluna-pos (primeiro lista-pl)) 2))
                                                          lista-pl))))
   (define (faz-posicoes-de-jogada-aux-coluna-cresc j lista-pc lista-pl)
     ; cria as posições de uma jogada que decorre ao longo duma coluna de forma crescente, ou seja, por exemplo a jogada ((1 . 1) (7 . 1))
    (if (pos=? (fim-jogada j) (primeiro lista-pl))
        (faz-posicoes-de-jogada (inverte lista-pc) (inverte lista-pl))
        (faz-posicoes-de-jogada-aux-coluna-cresc j 
                                                 (insere (faz-pos (+ 2 (linha-pos (primeiro lista-pc)))
                                                                  (coluna-pos (primeiro lista-pc))) 
                                                         lista-pc)
                                                 (insere (faz-pos (+ 2 (linha-pos (primeiro lista-pl)))
                                                                  (coluna-pos (primeiro lista-pl)))
                                                         lista-pl))))
  (define (faz-posicoes-de-jogada-aux-coluna-decresc j lista-pc lista-pl)
    ; cria as posições de uma jogada que decorre ao longo duma coluna de forma decrescente, ou seja, por exemplo a jogada ((7 . 1) (1 . 1))
    (if (pos=? (fim-jogada j) (primeiro lista-pl))
        (faz-posicoes-de-jogada (inverte lista-pc) (inverte lista-pl))
        (faz-posicoes-de-jogada-aux-coluna-decresc j 
                                                   (insere (faz-pos (- (linha-pos (primeiro lista-pc)) 2)
                                                                    (coluna-pos (primeiro lista-pc))) 
                                                           lista-pc)
                                                   (insere (faz-pos (- (linha-pos (primeiro lista-pl)) 2)
                                                                    (coluna-pos (primeiro lista-pl)))
                                                           lista-pl))))
  (if (and (jogada? j)
           (mesma-direccao? (inicio-jogada j) (fim-jogada j))
           (integer? (distancia (inicio-jogada j) (fim-jogada j)))
           (even? (distancia (inicio-jogada j) (fim-jogada j))))
      (if (if (< 6 (linha-pos (inicio-jogada j))) ; verifica se a jogada ocorre ao longo de uma linha ou de uma coluna, e caso as posições
                                                  ; tenham valores demasiado altos cria posições válidas a partir dos valores existentes. 
                                                  ; subtrai em vez de somar 1.
              (mesma-direccao? (fim-jogada j)
                               (cons (+ 1 (linha-pos (inicio-jogada j))) 
                                     (coluna-pos (inicio-jogada j))))
              (mesma-direccao? (fim-jogada j)
                               (cons (- 1 (linha-pos (inicio-jogada j))) 
                                     (coluna-pos (inicio-jogada j)))))
          (if (< (linha-pos (inicio-jogada j)) (linha-pos (fim-jogada j))) ; verifica se a jogada ocorre de modo crescente ou decrescente de
                                                                           ; acordo com as jogadas ao longo da coluna.
              (faz-posicoes-de-jogada-aux-coluna-cresc j
                                                       (insere (faz-pos (+ 1 (linha-pos (inicio-jogada j)))
                                                                        (coluna-pos (inicio-jogada j)))
                                                               (nova-lista))
                                                       (insere (faz-pos (+ 2 (linha-pos (inicio-jogada j)))
                                                                        (coluna-pos (inicio-jogada j)))
                                                               (nova-lista)))
              (faz-posicoes-de-jogada-aux-coluna-decresc j
                                                         (insere (faz-pos (- (linha-pos (inicio-jogada j)) 1)
                                                                          (coluna-pos (inicio-jogada j)))
                                                                 (nova-lista))
                                                         (insere (faz-pos (- (linha-pos (inicio-jogada j)) 2)
                                                                          (coluna-pos (inicio-jogada j)))
                                                                 (nova-lista))))
          (if (< (coluna-pos (inicio-jogada j)) (coluna-pos (fim-jogada j))) ; verifica se a jogada ocorre de modo crescente ou decrescente de
                                                                             ; acordo com as jogadas ao longo da linha.
              (faz-posicoes-de-jogada-aux-linha-cresc j 
                                                      (insere (faz-pos (linha-pos (inicio-jogada j))
                                                                       (+ 1 (coluna-pos (inicio-jogada j))))
                                                              (nova-lista))
                                                      (insere (faz-pos (linha-pos (inicio-jogada j))
                                                                       (+ 2 (coluna-pos (inicio-jogada j))))
                                                              (nova-lista)))
              (faz-posicoes-de-jogada-aux-linha-decresc j 
                                                        (insere (faz-pos (linha-pos (inicio-jogada j))
                                                                         (- 1 (coluna-pos (inicio-jogada j))))
                                                                (nova-lista))
                                                        (insere (faz-pos (linha-pos (inicio-jogada j))
                                                                         (- 2 (coluna-pos (inicio-jogada j))))
                                                                (nova-lista)))))
      (nova-lista)))