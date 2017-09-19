      ******************************************************************
      * Author: Joan Bagudà Rigol
      * Date: 17/09/2017
      * Purpose: COBOL Implementation of Peter Norvig's sudoku solver
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUDOKU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ProblemaFile ASSIGN TO "problema.txt"
             ORGANIZATION IS RECORD SEQUENTIAL.

           SELECT SolucioFile ASSIGN TO "solucio.txt"
             ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  ProblemaFile.
       01  CharIn PICTURE X.
           88 FideFitxer VALUE HIGH-VALUES.

       FD  SolucioFile.
       01  SolucioLinea.
           05 ProbLinea   PICTURE X(9).
           05 iProbLinea  PICTURE   99.
           05 DescrLinea  PICTURE X(30).
           05 TempsLinea  PICTURE Z(5)9.

       WORKING-STORAGE SECTION.
      ******Problem input ******
       01  EsDigit         PICTURE 9 VALUE 0.
       01  i               PICTURE 99.
       01  j               PICTURE 99.

      ******Statistics ******
       01  NumProbls      PICTURE 9999.
       01  NumResolt      PICTURE 9999.
       01  iProbl         PICTURE 9999.
       01  LlegirTemps.
           05 TempsH      PICTURE 99.
           05 TempsM      PICTURE 99.
           05 TempsS      PICTURE 99.
           05 TempsL      PICTURE 99.
       01  TMilis         PICTURE 9(9).
       01  Tinitot        PICTURE 9(9).
       01  Tfintot        PICTURE 9(9).
       01  Ttot           PICTURE 9(9).
       01  Tiniprbl       PICTURE 9(9).
       01  Tfinprbl       PICTURE 9(9).
       01  Tprbl          PICTURE 9(9).

      ******Table for grid reading and writing ******
       01  GridProbl.
           05 GridiFil occurs 9 times.
             10 GridiCol occurs 9 times.
               15 GridCasella picture X.
       01  GridProblLlista REDEFINES GridProbl.
           05 GridLlista PICTURE X OCCURS 81 TIMES.

       01  Tauler-Escriure.
           05 iFilaGrup occurs 9 times.
             10 iColGrup occurs 3 times.
               15 Grup picture X(3).
       01  Grp-sep-hor picture X(11) value '-----------'.

       01  TVals-Escriure.
           05 FilValEscr picture X(92) occurs 9 times.
       01  Grp-sep-hor-v picture X(92) values all '-'.
       01  Punter PICTURE 9(2).

      ******Indices to squares for procedure InitValues
       01  iFil  picture 99.
           88 FQuad1 values 1 through 3.
           88 FQuad2 values 4 through 6.
           88 FQuad3 values 7 through 9.
       01  iCol  picture 99.
           88 CQuad1 values 1 through 3.
           88 CQuad2 values 4 through 6.
           88 CQuad3 values 7 through 9.
       01  icas  picture 99.
       01  icas2 picture 99.

       01  iFil2 picture 99.
       01  iCol2 picture 99.
       01  d2    picture 99.

      ******State of the search (contradiction, solved or neither) *****
       01  Estat          picture 9 value 0.
           88 Contr       value 1.
           88 Resolt      value 2.

      ******Limits of the iteration over a box ******
       01  FQmin picture 99.
       01  FQmax picture 99.
       01  CQmin picture 99.
       01  CQmax picture 99.

       01  d picture 99.
       01  Cont  picture 99.
       01  Cont2 picture 99.

      ******Table for convertion digit to character
       01  CarDigits PICTURE X(9) VALUE '123456789'.
       01  FILLER    REDEFINES CarDigits.
           05 Digits OCCURS 9 TIMES PICTURE X.
       01  iDig      PICTURE 99.

      ******Table of possible values, counter for the number of posible
      ******values of a square and the only last possible value
       01  TVals.
           05 FILLER occurs 81 times.
               15 ValsCas.
                 20 ValsCasDig PICTURE 9 occurs 9 times.
               15 NumDigs picture 9 value 9.
               15 UnicNoNul picture 9 value 0.

      ******List of possible values tables for search iteration
       01  LlTVals OCCURS 100 TIMES.
           15 FILLER PICTURE 9(11) OCCURS 81 TIMES.
       01  uLlTv   PICTURE 999 VALUE 1.

      ******Look-up table of associated squares ('peers')
      ******and units of each square
       01  TAssoc.
           05 FILLER OCCURS 81 TIMES.
             15 AssocCas PICTURE 99 OCCURS 24 TIMES.

       01  TUnits REDEFINES TAssoc.
           05 FILLER OCCURS 81 TIMES.
             15 FILLER OCCURS 3 TIMES.
               20 Unitcas PICTURE 99 OCCURS 8 TIMES.

      ******A square with the minimum number of possible values
       01  Min       PICTURE 9.
       01  icasMin   PICTURE 99.

      ******List of values to assign at each search depth level
       01  LlNivAssignar OCCURS 1000 TIMES.
           05 LlAssignar OCCURS 9 TIMES.
             10 Assicas PICTURE 99.
             10 AssVal  PICTURE 99.
           05  pLlAss     PICTURE 9999 VALUE 1.
           05  uLlAss     PICTURE 9999 VALUE 1.
       01  Nivell PICTURE 999.

      ******List of values to eliminate
      ******pLl = Next square to process
      ******uLl = First empty position on the list
       01  LlVals    occurs 1000 times.
           05 Llicas      picture 99.
           05 LlVal       picture 99.
       01  pLl       picture 9999 value 1.
       01  uLl       picture 9999 value 1.

       PROCEDURE DIVISION.

      *****************************************************************
      ************* START AND MAIN LOOP *******************************
      *****************************************************************
       MAIN-PROCEDURE.

           ACCEPT LlegirTemps FROM TIME
           PERFORM CalculaMilisegons
           MOVE TMilis TO Tinitot

           PERFORM InitAssociades
           OPEN INPUT ProblemaFile
           OPEN OUTPUT SolucioFile
           MOVE 0 TO NumResolt
           PERFORM Llegir-problema

           PERFORM VARYING iProbl FROM 1 BY 1
                                  UNTIL iProbl>1000 OR FideFitxer

             DISPLAY 'Problema ' iProbl ' ... '
             ACCEPT LlegirTemps FROM TIME
             PERFORM CalculaMilisegons
             MOVE TMilis TO Tiniprbl

             MOVE 0 TO Estat
             PERFORM Init-valors
             IF Contr THEN
               PERFORM Escriu-sol-contr
             ELSE

               PERFORM Cerca

               ACCEPT LlegirTemps FROM TIME
               PERFORM CalculaMilisegons
               MOVE TMilis TO Tfinprbl

               COMPUTE Tprbl = Tfinprbl - Tiniprbl
               IF NOT Resolt THEN
                 PERFORM Escriu-sol-cap
                 DISPLAY 'Problema ' iProbl 'no solucio'
               ELSE PERFORM Escriu-sol-resolt
                 DISPLAY 'Problema ' iProbl 'resolt'
                 ADD 1 TO NumResolt
               END-IF
             END-IF

             PERFORM Llegir-problema
           END-PERFORM

           IF iProbl>1000 THEN DISPLAY 'Maxim 1000 problemes' END-IF
           SUBTRACT 1 FROM iProbl GIVING NumProbls
           PERFORM Escriu-sol-totals
           DISPLAY NumResolt ' resolts de ' NumProbls
           ACCEPT LlegirTemps FROM TIME
           PERFORM CalculaMilisegons
           COMPUTE Ttot = TMilis - Tinitot
           DISPLAY 'Temps total(ms): ' Ttot

           CLOSE ProblemaFile
           CLOSE SolucioFile
           STOP RUN.

      *****************************************************************
      ******* INITIALIZATION OF LOOK-UP TABLE OF PEERS AND UNITS ******
      *****************************************************************
       InitAssociades.
           PERFORM VARYING ifil FROM 1 BY 1 UNTIL ifil>9
             PERFORM VARYING icol FROM 1 BY 1 UNTIL icol>9
               COMPUTE icas = (ifil - 1) * 9 + icol
               MOVE 1 TO i
               PERFORM VARYING icol2 FROM 1 BY 1 UNTIL icol2>9
                 IF icol2 NOT = icol THEN
                     COMPUTE icas2 = (ifil - 1) * 9 + icol2
                     MOVE icas2 TO AssocCas(icas,i)
                     ADD 1 to i
                 END-IF
               END-PERFORM
               PERFORM VARYING ifil2 FROM 1 BY 1 UNTIL ifil2>9
                 IF ifil2 NOT = ifil THEN
                     COMPUTE icas2 = (ifil2 - 1) * 9 + icol
                     MOVE icas2 TO AssocCas(icas,i)
                     ADD 1 to i
                 END-IF
               END-PERFORM
               PERFORM Limits-quad
               PERFORM VARYING iFil2 FROM FQmin BY 1 UNTIL iFil2>FQmax
                 PERFORM VARYING iCol2 FROM CQmin BY 1 UNTIL iCol2>CQmax
                   IF ifil2 NOT = ifil OR icol2 NOT = icol THEN
                       COMPUTE icas2 = (ifil2 - 1) * 9 + icol2
                       MOVE icas2 TO AssocCas(icas,i)
                       ADD 1 to i
                   END-IF
                 END-PERFORM
               END-PERFORM
             END-PERFORM
           END-PERFORM.

      *****************************************************************
      ******** LIMITS OF THE ITERATION OVER A BOX, GIVEN A SQUARE *****
      *****************************************************************
       Limits-quad.
           EVALUATE TRUE
             WHEN FQuad1      MOVE 1 TO FQmin
                              MOVE 3 TO FQmax
             WHEN FQuad2      MOVE 4 TO FQmin
                              MOVE 6 TO FQmax
             WHEN FQuad3      MOVE 7 TO FQmin
                              MOVE 9 TO FQmax
           END-EVALUATE
           EVALUATE TRUE
             WHEN CQuad1      MOVE 1 TO CQmin
                              MOVE 3 TO CQmax
             WHEN CQuad2      MOVE 4 TO CQmin
                              MOVE 6 TO CQmax
             WHEN CQuad3      MOVE 7 TO CQmin
                              MOVE 9 TO CQmax
           END-EVALUATE.

      *****************************************************************
      ***************    TIME TO MILLISECONDS      ********************
      *****************************************************************
       CalculaMilisegons.
           COMPUTE TMilis = TempsL + TempsS * 1000 + TempsM * 60000 +
                            TempsH * 3600000.

      *****************************************************************
      **************  PROBLEM READING FROM FILE ***********************
      *****************************************************************
       Llegir-problema.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i>9 OR FideFitxer
             PERFORM VARYING j FROM 1 BY 1 UNTIL j>9 OR FideFitxer
               MOVE 0 TO EsDigit
               PERFORM UNTIL EsDigit = 1 OR FideFitxer
                 READ ProblemaFile
                     AT END SET FideFitxer TO TRUE
                            EXIT PERFORM
                 END-READ
                 INSPECT '0123456789.' TALLYING EsDigit FOR ALL CharIn
               END-PERFORM
               IF NOT FideFitxer THEN
                 INSPECT CharIn CONVERTING '0' TO '.'
                 MOVE CharIn TO GridCasella(i,j)
               END-IF
             END-PERFORM
           END-PERFORM.

      *****************************************************************
      ************** WRITING PROCEDURES *******************************
      *****************************************************************
       Escriu-sol-contr.
           MOVE 'Problema ' TO ProbLinea
           MOVE iProbl TO iProbLinea
           MOVE ': Contradiccio inicialitzant' TO DescrLinea
           WRITE SolucioLinea.

       Escriu-sol-cap.
           MOVE 'Problema ' TO ProbLinea
           MOVE iProbl TO iProbLinea
           MOVE ': Cap solucio trobada' TO DescrLinea
           MOVE TPrbl TO TempsLinea
           WRITE SolucioLinea.

       Escriu-sol-resolt.
           MOVE 'Problema ' TO ProbLinea
           MOVE iProbl TO iProbLinea
           MOVE ': Solucio trobada' TO DescrLinea
           MOVE TPrbl TO TempsLinea
           WRITE SolucioLinea.
           MOVE SPACES TO SolucioLinea
           PERFORM VARYING i FROM 1 BY 1 UNTIL i>9
             PERFORM VARYING j FROM 1 BY 1 UNTIL j>9
               COMPUTE icas = (i - 1)*9 + j
               MOVE UnicNoNul(icas) TO GridCasella(i,j)
             END-PERFORM
           END-PERFORM.

           MOVE GridProbl TO Tauler-Escriure.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i>9
             STRING Grup(i,1)
                    '|'
                    Grup(i,2)
                    '|'
                    Grup(i,3)
                    INTO SolucioLinea
             WRITE SolucioLinea
             IF i=3 OR 6 THEN
               MOVE Grp-sep-hor TO SolucioLinea
               WRITE SolucioLinea
             END-IF
           END-PERFORM.

           MOVE SPACES TO SolucioLinea
           WRITE SolucioLinea.

       Escriu-sol-totals.
           STRING NumResolt
                  ' resolts de '
                  NumProbls
                  INTO SolucioLinea
           WRITE SolucioLinea.

      *****************************************************************
      ******* ASSIGN INITIAL POSSIBLE VALUES GIVEN A PROBLEM **********
      *****************************************************************
       Init-valors.
           PERFORM VARYING icas FROM 1 BY 1 UNTIL icas>81
             PERFORM Assigna-tots
             PERFORM Elimina-valors
           END-PERFORM.

           PERFORM VARYING icas FROM 1 BY 1 UNTIL icas>81
             MOVE 0 TO Cont
             INSPECT ValsCas(icas) TALLYING Cont FOR ALL '0'
             SUBTRACT Cont FROM 9 GIVING NumDigs(icas)
             IF NumDigs(icas) = 1 THEN
                 PERFORM VARYING Cont FROM 1 BY 1
                         UNTIL ValsCasDig(icas,Cont) NOT = 0
                 END-PERFORM
                 MOVE Cont TO UnicNoNul(icas)
           END-PERFORM.

       Assigna-tots.
           MOVE '123456789' TO ValsCas(icas).

       Assigna-inicials.
           PERFORM VARYING icas FROM 1 BY 1 UNTIL icas>81
             IF GridLlista(icas) NOT = '.' THEN
               MOVE GridLlista(icas) TO d
               MOVE icas TO icas2
               PERFORM Assigna
             END-IF
           END-PERFORM.

       Elimina-valors.
           PERFORM VARYING i FROM 1 BY 1 UNTIL i>3
             PERFORM VARYING j FROM 1 BY 1 UNTIL j>8 OR Cont>1
               MOVE UnitCas(icas,i,j) TO icas2
               PERFORM Borra-igual
             END-PERFORM
           END-PERFORM.

       Borra-igual.
           IF  icas NOT = icas2
               AND GridLlista(icas2) NOT = '.'
               THEN      INSPECT ValsCas(icas)
                         REPLACING ALL GridLlista(icas2) BY '0'
           END-IF.
           IF GridLlista(icas) NOT = '.' THEN
               MOVE 0 TO Cont
               INSPECT ValsCas(icas)
                       TALLYING Cont FOR ALL GridLlista(icas)
               IF Cont = 0 THEN
                   SET Contr TO TRUE
               ELSE
                   EVALUATE GridLlista(icas)
                     WHEN '1' MOVE '100000000' TO ValsCas(icas)
                     WHEN '2' MOVE '020000000' TO ValsCas(icas)
                     WHEN '3' MOVE '003000000' TO ValsCas(icas)
                     WHEN '4' MOVE '000400000' TO ValsCas(icas)
                     WHEN '5' MOVE '000050000' TO ValsCas(icas)
                     WHEN '6' MOVE '000006000' TO ValsCas(icas)
                     WHEN '7' MOVE '000000700' TO ValsCas(icas)
                     WHEN '8' MOVE '000000080' TO ValsCas(icas)
                     WHEN '9' MOVE '000000009' TO ValsCas(icas).

      ****************************************************************
      ******* ELIMINATES THE POSSIBLE VALUES CONTAINED IN LlVals *****
      ******* ADDING TO THE LIST NEW VALUES TO ELIMINATE ACCORDING ***
      ******* TO THE TWO MAIN RULES:                             *****
      ******* (1) If a square has only one possible value, eliminate *
      ******* that value from all the associated squares         *****
      ******* (2) If a unit only has one possible square for the *****
      ******* newly elimnated value,
      ******* assign that value to that square                   *****
      ****************************************************************
       Elimina.
           MOVE 1 TO pLl
      *****Iterate until the list is empty or a contradiction is found
           PERFORM UNTIL pLl = uLl OR Contr
             MOVE Llicas(pLl)  TO icas
             MOVE LlVal(pLl)   TO d
             IF ValsCasDig(icas,d) = 0 THEN
                 ADD 1 TO pLl
                 EXIT PERFORM CYCLE
             END-IF
             MOVE 0 TO ValsCasDig(icas,d)
             SUBTRACT 1 FROM NumDigs(icas)

      *******Check contradiction
             IF NumDigs(icas) = 0 THEN
               SET Contr TO TRUE
               EXIT PERFORM
             END-IF

      *******RULE 1
              IF NumDigs(icas) = 1 THEN
               PERFORM Afegir-elements-elim
             END-IF

      *******RULE 2
             PERFORM VARYING i FROM 1 BY 1 UNTIL i>3
               MOVE 0 TO Cont
               PERFORM VARYING j FROM 1 BY 1 UNTIL j>8 OR Cont>1
                 IF ValsCasDig(UnitCas(icas,i,j),d) = d THEN
                     ADD 1 TO Cont
                     MOVE UnitCas(icas,i,j) TO icas2
                 END-IF
               END-PERFORM
               IF Cont = 1 THEN
                   PERFORM Assigna
               ELSE IF Cont = 0 THEN
                   SET Contr TO TRUE
                   EXIT PERFORM
                   END-IF
               END-IF
             END-PERFORM

           IF NOT Contr THEN PERFORM Comproba-resolt

      *******Pointer to square to be processed in the next iteration
             ADD 1 TO pLl
           END-PERFORM.

      *****************************************************************
      *************** CHECK IF THE PROBLEM IS SOLVED ******************
      *****************************************************************
       Comproba-resolt.
           SET Resolt TO TRUE
           PERFORM VARYING icas FROM 1 BY 1 UNTIL icas>81 OR NOT Resolt
             IF NumDigs(icas)>1 THEN
                 MOVE 0 TO Estat
             END-IF
           END-PERFORM.

      *****************************************************************
      ******* PUT TO THE LIST SQUARES ASSOCIATED WITH icas ************
      ******* WITH ITS ONLY REMAINING VALUE TO BE ELIMINATED **********
      *****************************************************************
       Afegir-elements-elim.
           PERFORM VARYING Cont FROM 1 BY 1
                   UNTIL ValsCasDig(icas,Cont) NOT = 0
           END-PERFORM
           MOVE Cont to d2
           MOVE d2 TO UnicNoNul(icas)
      *    Recorrem associades
           PERFORM VARYING i FROM 1 BY 1 UNTIL i>24
             MOVE AssocCas(icas,i) TO icas2
             IF ValsCasDig(icas2,d2) = d2 THEN
               MOVE icas2 TO Llicas(uLl)
               MOVE    d2 TO LlVal(uLl)
               ADD      1 TO uLl
             END-IF
           END-PERFORM.

      ****** Assign a value to a square (eliminate all values but that)
       Assigna.
           PERFORM VARYING iDig FROM 1 BY 1 UNTIL iDig>9
             IF iDig = d THEN EXIT PERFORM CYCLE end-if
             IF ValsCasDig(icas2,iDig) NOT = 0
             THEN
               MOVE icas2         TO Llicas(uLl)
               MOVE iDig          TO LlVal(uLl)
               ADD  1             TO uLl
             END-IF
           END-PERFORM.

      ****** Find a square with a minimum of possible values ******
       Troba-minim.
           MOVE 9 TO Min.
           MOVE 1 TO icasmin.

           PERFORM VARYING icas FROM 1 BY 1 UNTIL icas > 81
               MOVE NumDigs(icas) TO Cont
               IF Cont < Min AND Cont > 1 THEN
                   MOVE icas TO icasMin
                   MOVE Cont TO Min
               END-IF
           END-PERFORM.

      ****** ADD TO THE ASSIGN LIST
       Afegeix-assignar.
           MOVE 1 TO pLlAss(Nivell)
           MOVE 1 TO uLlAss(Nivell)
           PERFORM VARYING Cont FROM 1 BY 1 UNTIL Cont>9
             IF ValsCasDig(icasmin,Cont) NOT = 0 THEN
               MOVE icasMin TO Assicas(Nivell,uLlAss(Nivell))
               MOVE Cont TO AssVal(Nivell,uLlAss(Nivell))
               ADD 1 TO uLlAss(Nivell)
             END-IF
           END-PERFORM.

      ******  APPLY ASSIGNATION (ADD TO ELIMINATE LIST) *************
       Aplica-assignacio.
      *    Apliquem assignació continguda a (Nivell,pLlAss(nivell))
           MOVE 1 TO pLl
           MOVE 1 TO uLl
           MOVE Assicas(Nivell,pLlAss(Nivell)) TO icas2
           MOVE AssVal(Nivell,pLlAss(Nivell))  TO d
           PERFORM Assigna.
           PERFORM Elimina.

      ******** SEARCH ************************************************
       Cerca.

           MOVE 1 TO Nivell
           PERFORM Troba-minim.
           PERFORM Afegeix-assignar.
           MOVE TVals TO LlTVals(1).

      ***** ONE OF THE VALUES TRIED ON THE FIRST SQURE IS CORRECT
      ***** IF AND ONLY IF THE PROBLEM HAS SOLUTION
      ***** LOOP CONDITION: UNTIL SOLVED OR OUT OF VALUES AT LEVEL 1
           PERFORM UNTIL Resolt OR (pLlAss(1) = uLlAss(1) AND Nivell=1)
             MOVE LlTVals(Nivell) TO TVals

      ***** GO UP A LEVEL IF THERE ARE NO PENDING ASSIGNATIONS ON THE
      ***** LIST
             IF pLlAss(Nivell) = uLlAss(Nivell) THEN
                 SUBTRACT 1 FROM Nivell
                 EXIT PERFORM CYCLE
             END-IF

      ***** APPLY THE ASSIGNATION CONTAINED AT (Nivell,pLlAss)
             PERFORM Aplica-assignacio
             ADD 1 TO pLlAss(Nivell)
             EVALUATE True
             WHEN Resolt      EXIT PERFORM
             WHEN Contr       MOVE 0 TO Estat
             WHEN OTHER
      ***** NEITHER CONTRADICTION NOR SOLVED; WE GO DOWN ONE LEVEL
               ADD 1 TO Nivell
               PERFORM Troba-minim
               PERFORM Afegeix-assignar
               MOVE TVals TO LlTVals(Nivell)
             END-EVALUATE
           END-PERFORM.
       END PROGRAM SUDOKU.
