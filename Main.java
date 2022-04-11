package v1;

import java.util.Arrays;

public class Main {

	public static void main(String[] args) {
		
		int[] solved = new int[9];
		int[] unsolved = new int[9];
		int ctr = 8;
		
		int[] testing = {0,0,0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,0,0,
						0,0,0,0,0,0,0,0,0};
		int manipulate = 0;
		
		Board sudoku;

		while (true) {
			manipulate = 0;
			ctr = 8;
			sudoku = new Board(testing, false);
			if (Solver.solve(sudoku, false)) {
				solved[ctr] ++;
				while (solved[ctr] == 1000000000) {
					solved[ctr--] = 0;
					solved[ctr] ++;
				}
			}else {
				unsolved[ctr] ++;
				while (unsolved[ctr] == 1000000000) {
					unsolved[ctr--] = 0;
					unsolved[ctr] ++;
				}
			}
			testing[manipulate] ++;
			while (testing[manipulate] == 10) {
				if (manipulate == 80) {
					System.out.println("Finished without any errors!");
					System.exit(0);
				}else if ((manipulate % 10 == 0) && (manipulate != 0)) {
					System.out.println(manipulate);
				}
				testing[manipulate++] = 0;
				testing[manipulate] ++;
			}
		}
		

		
		/*
		Board b1;
		Board b2;
		Board b3;
		int[] Sample1 = {0,0,0,0,0,0,1,0,0
						,0,0,0,0,2,3,9,6,8
						,0,0,0,0,6,0,0,3,2
						,0,7,0,6,3,0,8,0,0
						,8,0,0,0,5,0,0,0,3
						,0,0,3,0,9,2,0,4,0
						,2,3,0,0,4,0,0,0,0
						,9,8,4,3,1,0,0,0,0
						,0,0,6,0,0,0,0,0,0};
		int[] Sample2 = {0,0,1,0,0,4,0,0,3
						,6,0,0,2,0,0,4,0,0
						,4,7,0,8,3,0,5,1,0
						,0,0,7,0,0,8,0,0,4
						,5,0,0,9,0,0,8,0,0
						,9,8,0,7,5,0,2,6,0
						,0,0,3,0,0,9,0,0,8
						,7,0,0,1,0,0,3,0,0
						,2,9,0,3,7,0,1,4,0};
		int[] Sample3 = {};
		
		b1 = new Board(Sample1, true);
		Solver.solve(b1, true);
		b1.status();
		
		b2 = new Board(Sample2, true);
		Solver.solve(b2, true);
		b2.status();
		
		b3 = new Board(Sample3, true);
		Solver.solve(b3, true);
		b3.status();
		*/
	}

}
