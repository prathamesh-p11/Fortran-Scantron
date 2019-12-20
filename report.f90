!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine read_rec(no_q, no_stud)
	implicit none 
	integer :: no_q 										!number of questions
	integer :: no_stud 										!Number of students
	integer :: eofile										!End of file 
	integer, dimension(no_stud, no_q+1) :: stud_ans			!read and store student answers in this matrix 
	integer, dimension(no_q) :: ans_key						!correct answers array
	integer, dimension(no_stud,2) :: marksheet				!final marksheet of each student
	integer, dimension(no_q+1) :: answers					!answers of individual student
	integer :: scores										!get total score from function
	integer, dimension(no_stud) :: s   						!array to store scores    
	integer, dimension(no_stud) :: v   						!greek v for frequency
	integer :: temp, temp2									!temporary var
	real :: average 										!class average
	integer :: i,j,k 										!iterators
	integer, external :: calc_score 						!function to calc total score


!----------------------Read answer keys and student responses--------------------------------

	read(8,*)		!skip line with number of questions
	
	read(8,*,IOSTAT = eofile) ans_key
	
	do i=1,no_stud
		read(8,*,IOSTAT = eofile) stud_ans(i,:)
		if(eofile < 0) then
			print *,""
		end if
	end do

!-----------------------------------------------------------------------------------------------


!------------------------------------Print marksheet--------------------------------------------

	do i=1,no_stud
		answers = stud_ans(i,:)
		scores = calc_score(ans_key, answers, no_q, no_stud)		!Call function to calc score of individual student
		marksheet(i,1) = answers(1)									!store roll no
		marksheet(i,2) = scores										!store total score
	end do

	print *,""
	print *, "    Student ID     	   Score"
	print *, "==============================="
	
	do k=1,no_stud
			print *, marksheet(k,1), "     ", marksheet(k,2)
	end do

	print *, "==============================="
	print *,"Tests Graded =   ", no_stud
!------------------------------------------------------------------------


!---------------------------Score Frequency------------------------------

!initialize score matrix with all scores(no roll)
	do i=1, no_stud
		s(i) = marksheet(i,2)
	end do

!--------------------------initialize frequency matrix with all 1--------------------------
	do i=1, no_stud
		v(i) = 1
	end do
	
!--------------------------calc frequencies of scores--------------------------
	do i=1,no_stud
		do j=1,i
			if(s(i) == s(j) .AND. i/=j) then
				v(i) = v(i) + 1
				v(j) = 0
			end if
		end do
	end do

!--------------------------Bubble sort frequency and marks arrays--------------------------
	temp = 0
	temp2 = 0
	do i=1,no_stud
		do j=1,no_stud - i
			if(s(j) < s(j+1)) then
			temp = s(j)
			temp2 = v(j)
			s(j) = s(j+1)
			v(j) = v(j+1)
			s(j+1) = temp
			v(j+1) = temp2 
			end if
		end do
	end do

!--------------------------print frequency and average--------------------------
	temp = 0
	print *, "==============================="
	
	print *,"	Score 	  Frequency"
	print *, "==============================="
	
	do i=1,no_stud
		if(v(i)/=0) then
			print *,s(i),v(i)
		end if

		temp = temp + (s(i) * v(i))
	end do
	print *, "==============================="
	
average = real(temp)/no_stud
write (*, "(A17 , f8.2)") " Class Average = ", average
!------------------------------------------------------------------------


end subroutine read_rec


!------------------------------------------------------------------------
!------------------------------------------------------------------------
function calc_score(ans_key, answers, no_q, no_stud)
	implicit none
	integer :: no_q
	integer :: no_stud
	integer, dimension(no_q+1) :: answers
	integer, dimension(no_q) :: ans_key
	integer :: marks
	integer :: calc_score
	integer :: m

	!------------------------Calc total score--------------------------
	marks = 0
	marks = 100 / no_q
	calc_score = 0

	do m=1,no_q
				if (answers(m+1) == ans_key(m)) then
					calc_score = calc_score + marks
				end if
	end do 

end function calc_score


!------------------------------------------------------------------------
!------------------------------------------------------------------------


program report

	!-------------------------Variables-------------------------------------
	character(len = 50) :: file_name
	integer :: no_questions
	integer :: no_students
	integer :: eofile
		
	no_questions = 0
	countline = 0
	no_students = 0
	!------------------------------------------------------------------------


	!----------------------GET FILE NAME AND OPEN---------------------------
	print *, "Enter file name :"
	read *,file_name
	!------------------------------------------------------------------------
	
	!---------------------------READING FILE---------------------------
	open(8, file= file_name, status = 'old')
	read(8,*) no_questions
	!---------------------------------------------------------------------------------


	
	!----------------------TO COUNT NUMBER OF STUDENTS---------------------
	rewind(8)
	read(8,*)
	read(8,*)
	!skipped two lines of no of q and ans keys
	
	do
		read(8, '(A)', IOSTAT=eofile) charline 
		if (eofile < 0) then
			exit
		
		else
			no_students = no_students + 1
		end if
	end do

	rewind(8)				!place file pointer to the start of file 
	!------------------------------------------------------------------------
	

	!----------------------IF NO STUDENTS PRESENT-----------------------------
	if (no_students < 1) then
		stop 'ERROR!!!!..............STUDENTS = 0...........Terminating program.....'
	end if
	!------------------------------------------------------------------------
	

	call read_rec(no_questions, no_students)

	close(8)

end program report

!------------------------------------------------------------------------
!------------------------------------------------------------------------
!------------------------------------------------------------------------