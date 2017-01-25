
(require (lib "misc.ss" "swindle")
         (lib "list.ss")
         (lib "lset.ss" "srfi" "1")
         (lib "trace.ss"))

;;
;; Ficheiro com código a testar
;;
;; Nota: o ficheiro tem que que estar na mesma directoria ou 
;;       então tem que ser fornecido o caminho até ao ficheiro.
(load "FP1011-parte1-grupo0.scm")

;; TAI teste
(define (cria-teste nome forma erro?)
  (cons erro? (cons nome forma)))

;; Nome do teste
(define (nome-teste teste)
  (cadr teste))

;; Código a ser avaliado
(define (forma-teste teste)
  (cddr teste))

;; Indica se a avaliação do código deve gerar um erro de execução
(define (erro?-teste teste)
  (car teste))


;; Definição dos testes
(define *ts*
  (list
   ;; TAI posicao
   (cria-teste "teste posicao 01" '(faz-pos 0 10) #t)
   (cria-teste "teste posicao 02" '(= (linha-pos (faz-pos 0 1)) 0) #f)
   (cria-teste "teste posicao 03" '(= (coluna-pos (faz-pos 0 1)) 1) #f)
   (cria-teste "teste posicao 04" '(pos? (faz-pos 1 2)) #f)
   (cria-teste "teste posicao 05" '(not (pos? (cons 8 2))) #f)
   (cria-teste "teste posicao 06" '(not (pos? (list 8 2))) #f)
   (cria-teste "teste posicao 07" '(not (pos? 3)) #f)
   (cria-teste "teste posicao 08" '(pos=? (faz-pos 1 2) (faz-pos 1 2)) #f)
   (cria-teste "teste posicao 09" '(not (pos=? (faz-pos 1 5) (faz-pos 1 2))) #f)
   (cria-teste "teste posicao 10" '(= (distancia (faz-pos 1 1) (faz-pos 1 3)) 2) #f)
   (cria-teste "teste posicao 11" '(mesma-direccao? (faz-pos 1 5) (faz-pos 1 2)) #f)   
   (cria-teste "teste posicao 12" '(not (mesma-direccao? (faz-pos 1 5) (faz-pos 4 2))) #f)   
   (cria-teste "teste posicao 13" '(adjacentes? (faz-pos 3 4) (faz-pos 4 4)) #f)   
   (cria-teste "teste posicao 14" '(not (adjacentes? (faz-pos 6 4) (faz-pos 4 4))) #f)   
   (cria-teste "teste posicao 15" '(lset-iguais? 
                                    (adjacentes (faz-pos 3 4)) 
                                    (list (faz-pos 4 4) (faz-pos 2 4) 
                                          (faz-pos 3 5) (faz-pos 3 3)))
                                    #f)  
   ;; TAI jogada
      
   (cria-teste "teste jogada 01" '(faz-jogada 0 (faz-pos 0 0)) #t)
   (cria-teste "teste jogada 02" 
               '(equal? 
                 (inicio-jogada (faz-jogada (faz-pos 0 4) (faz-pos 0 0)))
                 (faz-pos 0 4))
               #f)
   (cria-teste "teste jogada 03" 
               '(equal? 
                 (fim-jogada (faz-jogada (faz-pos 0 4) (faz-pos 0 0)))
                 (faz-pos 0 0))
               #f)
   (cria-teste "teste jogada 04" 
               '(jogada? (faz-jogada (faz-pos 0 4) (faz-pos 0 0)))
               #f)
   (cria-teste "teste jogada 05" 
               '(not (jogada? 4))
               #f)
   (cria-teste "teste jogada 06" 
               '(jogada-nula? (faz-jogada (faz-pos 0 0) (faz-pos 0 0)))
               #f)
   (cria-teste "teste jogada 07" 
               '(not (jogada-nula? (faz-jogada (faz-pos 0 4) (faz-pos 0 0))))
               #f)

      ;; TAI posicoes de jogada

   (cria-teste "teste posicoes de jogada 01" 
               '(faz-posicoes-de-jogada (list (faz-pos 1 2) (faz-pos 3 2))
                                        (faz-pos 6 2))
               #t)
   (cria-teste "teste posicoes de jogada 02" 
               '(lset-iguais?
                 (pecas-capturadas
                  (faz-posicoes-de-jogada (list (faz-pos 1 2) (faz-pos 3 2))
                                          (list (faz-pos 5 2) (faz-pos 6 2))))
                  (list (faz-pos 1 2) (faz-pos 3 2)))
               #f)
   (cria-teste "teste posicoes de jogada 03" 
               '(lset-iguais?
                 (posicoes-livres
                  (faz-posicoes-de-jogada (list (faz-pos 1 2) (faz-pos 3 2))
                                          (list (faz-pos 5 2) (faz-pos 6 2))))
                  (list (faz-pos 5 2) (faz-pos 6 2)))
               #f)
   (cria-teste "teste posicoes de jogada 04" 
               '(lset-iguais?
                 (pecas-capturadas
                  (posicoes-de-jogada (faz-jogada (faz-pos 7 7) (faz-pos 1 7))))
                  (list (faz-pos 6 7) (faz-pos 4 7) (faz-pos 2 7)))
               #f)
   (cria-teste "teste posicoes de jogada 05" 
               '(lset-iguais?
                 (posicoes-livres
                  (posicoes-de-jogada (faz-jogada (faz-pos 7 7) (faz-pos 1 7))))
                  (list (faz-pos 5 7) (faz-pos 3 7) (faz-pos 1 7)))
               #f)
   

   ))
 
(define  (lset-iguais? l1 l2)
  (and (= (length l1) (length l2))
          (lset= equal? l1 l2)))


(define (testa testes)
  (dolist (teste testes)
          (let ((res (no-errors* (eval (forma-teste teste)))))
            (newline)
            (display (nome-teste teste))
            (cond ((struct? res) (if (erro?-teste teste)
                                     (display " - Passou.")
                                     (display " - FALHOU.")))
                  ((erro?-teste teste) (display " - FALHOU."))
                  ((eq? res #t) (display " - Passou."))
                  (else (display " - FALHOU."))))))

(testa *ts*)
                