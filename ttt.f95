! Program that plays tic-tac-toe
! plays versus computer

PROGRAM TICTACTOE   
	
    call playTicTacToe()
   
END program TICTACTOE
	
subroutine playTicTacToe()
    implicit none
    character, dimension(3,3) :: TICTAC(3,3)
    logical :: OVER
    character :: WINNER 
    integer :: TURN , MOVE,i,j , getMove
    
    write(*,*)""
    write(*,*)"Play tic-tac-toe. Press 1-9 to play."
    write(*,*)""
    write(*,*)"     1  |  2  |  3  "
    write(*,*)"   -----|-----|-----"
    write(*,*)"     4  |  5  |  6  "
    write(*,*)"   -----|-----|-----"
    write(*,*)"     7  |  8  |  9  "
    write(*,*)""

    do i=1,3
        do j=1,3
            TICTAC(i,j) = ' '
        end do
    end do
    
    do 
        
        MOVE = getMove(TURN,MOVE,TICTAC)
                    
        
        
        
        select case (MOVE)
            case(1)
                TICTAC(1,1) = 'x'
            case(2)
                TICTAC(1,2) = 'x'
            case(3)
                TICTAC(1,3) = 'x'
            case(4)
                TICTAC(2,1) = 'x'
            case(5)
                TICTAC(2,2) = 'x'
            case(6)
                TICTAC(2,3) = 'x'
            case(7)
                TICTAC(3,1) = 'x'
            case(8)
                TICTAC(3,2) = 'x'
            case(9)
                TICTAC(3,3) = 'x'
        end select
        
        do   
            call showBoard(TICTAC,TURN)
            call CHKOVR(TICTAC,OVER,WINNER)
            if (OVER .eqv. .true.)then
                exit
            end if
            if (TURN /= 0)then 
                exit
            end if
            turn = 1
            call pickMove(TICTAC)
        end do
        call CHKOVR(TICTAC,OVER,WINNER)
        if (OVER .eqv. .true.) exit
        
    end do
    
    if(WINNER == 'D')then
        write(*,*)"DRAW"
    else
        write(*,*)"Winner is:",WINNER
    end if
	
end

FUNCTION getMove(TURN,MOVE,TICTAC)
    integer :: TURN , MOVE, error,getMove
    character, dimension(3,3) :: TICTAC(3,3)
    logical :: CHKPLAY
    do
        TURN = 0
        write (*,*)"your turn"
        read(*,'(i2)',iostat=error) MOVE
        if(error /= 0)then
            write(*,*)"invalid input, integers only"
        else if(MOVE < 1 .or. MOVE > 9)then
            write(*,*)"wrong input, numbers 1-9 only"
        else if(CHKPLAY(TICTAC,MOVE) .eqv. .false.)then
            write(*,*)"box already filled, choose another" 
        else
            getMove = MOVE
            return              
        end if
    end do 
end 

LOGICAL FUNCTION CHKPLAY(TICTAC,MOVE)
    character, dimension(3,3) :: TICTAC(3,3)
    INTEGER :: MOVE
      
    CHKPLAY = .false.
      
    IF (MOVE==1 .and. TICTAC(1,1) == ' ')then
        CHKPLAY = .true.
    ELSE IF (MOVE == 2 .and. TICTAC(1,2) == ' ')then
        CHKPLAY = .true.
    ELSE IF (MOVE == 3 .and. TICTAC(1,3) == ' ')then
        CHKPLAY = .true.
    ELSE IF (MOVE == 4 .and. TICTAC(2,1) == ' ')then
        CHKPLAY = .true.
    ELSE IF (MOVE == 5 .and. TICTAC(2,2) == ' ')then
        CHKPLAY = .true.
    ELSE IF (MOVE == 6 .and. TICTAC(2,3) == ' ')then
        CHKPLAY = .true.
    ELSE IF (MOVE == 7 .and. TICTAC(3,1) == ' ')then
        CHKPLAY = .true.
    ELSE IF (MOVE == 8 .and. TICTAC(3,2) == ' ')then
        CHKPLAY = .true.
    ELSE IF (MOVE == 9 .and. TICTAC(3,3) == ' ')then
        CHKPLAY = .true.
    end if
 
END
    
subroutine showBoard(TICTAC,TURN)
    implicit none
    character, dimension(3,3) :: TICTAC(3,3)
    INTEGER :: TURN , i,j
    character(36)::format
    format = "(3X,A1,2X,'|',2X,A1,2X,'|',2X,A1,2X)"
    
    write(*,*)""
    if(TURN==0) write(*,*)"After your turn:"
    if(TURN==1) write(*,*)"After my turn:"
    
    do i =1,3
        write(*,format)(TICTAC(i,j),j=1,3)
        if(i /= 3) write(*,*)"-----|-----|-----"
    end do
    write(*,*)""
    
end
    
SUBROUTINE CHKOVR(TICTAC, OVER, WINNER)
!      
! CHECK IF TIC-TAC-TOE IS OVER AND DETERMINE WINNER (IF ANY)
!
! ARGUMENT DEFINITIONS --
!   INPUT ARGUMENTS
!     TICTAC - REPRESENTS THE CURRENT STATE OF THE BOARD GAME
!   OUTPUT ARGUMENTS
!     OVER - INDICATES WHETHER OR NOT GAME IS OVER
!     WINNER - INDICATES THE WINNER (O OR X) OR A DRAW (D)
!
      
    character, dimension(3,3) :: TICTAC(3,3)
    character :: WINNER
    LOGICAL :: OVER
!      
! SUBROUTINE PARAMETERS
    CHARACTER, PARAMETER :: BLANK = ' ', DRAW = 'D'
!
! FUNCTIONS USED
    LOGICAL :: SAME
!
! LOCAL VARIABLES
    LOGICAL :: DSAME
    INTEGER :: IR, IC
! ASSUME GAME IS OVER AT START
    OVER = .TRUE.
! 
! CHECK FOR A WINNER
! CHECK ROWS FOR A WINNER
    DO IR=1,3
        IF (SAME(TICTAC(IR,1), TICTAC(IR,2), TICTAC(IR,3))) THEN
            WINNER = TICTAC(IR,1)
            RETURN
        ENDIF
   END DO
! NO WINNER BY ROWS, CHECK COLUMNS FOR A WINNER
    DO IC=1, 3
        IF (SAME(TICTAC(1,IC), TICTAC(2,IC), TICTAC(3,IC))) THEN
            WINNER = TICTAC(1,IC)
            RETURN
        ENDIF
    END DO
! NO WINNER BY ROWS OR COLUMNS, CHECK DIAGONALS FOR A WINNER
    DSAME = SAME(TICTAC(1,1), TICTAC(2,2), TICTAC(3,3)) .OR. SAME(TICTAC(1,3), TICTAC(2,2), TICTAC(3,1))
    IF (DSAME) THEN
        WINNER = TICTAC(2,2)
        RETURN
    END IF
! NO WINNER AT ALL. SEE IF GAME IS A DRAW
! CHECK EACH ROW FOR AN EMPTY SPACE
    DO IR = 1, 3
        DO IC = 1, 3
            IF (TICTAC(IR,IC) .EQ. BLANK) THEN
                OVER = .FALSE.
                RETURN
            END IF
        END DO
    END DO
!
! NO BLANK FOUND, GRAME IS A DRAW
      WINNER = DRAW

    RETURN 
END   

logical function SAME(ONE, TWO, THREE)
    character :: ONE, TWO, THREE
    if(ONE == 'x' .and. TWO=='x' .and. THREE=='x')then
        SAME = .true.
    else if(ONE=='o' .and. TWO=='o' .and. THREE=='o')then
        SAME = .true.
    else
        SAME = .false.
    end if
end

subroutine pickMove(TICTAC)
    implicit none  
    character, dimension(3,3) :: TICTAC(3,3)
    integer :: PSUM(8) , PATHS(3,8), FIELD(9,2)
    data PATHS /1,4,7,2,5,8,3,6,9,1,2,3,4,5,6,7,8,9,1,5,9,3,5,7/
    data FIELD /1,2,3,1,2,3,1,2,3,1,1,1,2,2,2,3,3,3/
    !data PSUM /0,0,0,0,0,0,0,0/
    integer :: i,j,x,y, total, randx, randy
    
    ! calculate the paths 
    do i=1,8
        PSUM(i) = 0
        do j=1,3
            x = FIELD(PATHS(j,i),1)
            y = FIELD(PATHS(j,i),2)
            if(TICTAC(x,y) == ' ')then
                total = 0
                PSUM(i) = total + PSUM(i)
            else if(TICTAC(x,y) == 'x')then
                total = 1
                PSUM(i) = total + PSUM(i)
            else if(TICTAC(x,y) == 'o')then
                total = 4
                PSUM(i) = total + PSUM(i)
            end if
        end do
    end do

    ! checks if it can win the game
    do i = 1,8
        if(PSUM(i) == 8) then
            do j=1,3
                x = FIELD(PATHS(j,i),1)
                y = FIELD(PATHS(j,i),2)
                if(TICTAC(x,y) == ' ')then
                    TICTAC(x,y) = 'o'
                    return
                end if
            end do
        end if
    end do
    
    !checks if it can stop you
    do i = 1,8
        if(PSUM(i) == 2) then
            do j=1,3
                x = FIELD(PATHS(j,i),1)
                y = FIELD(PATHS(j,i),2)
                if(TICTAC(x,y) == ' ')then
                    TICTAC(x,y) = 'o'
                    return
                end if
                
            end do
        end if
    end do
    
    !random placement
    do
        randx = int(rand(0)*3)+1
        randy = int(rand(0)*3)+1
        if(TICTAC(randx,randy) == ' ')then
            TICTAC(randx,randy) = 'o'
            return
        end if
    end do
    
    return
end
    

