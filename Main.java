package v1;

public class Main {

	public static void main(String[] args) {
		
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
			sudoku = new Board(testing, false);
			sudoku.solve(false);
			testing[manipulate] ++;
			if (testing[manipulate] == 10) {
				if (manipulate == 80) {
					break;
				}
				testing[manipulate++] = 0;
			}
		}
		
		System.out.println("Finished without any errors!");
		
	}

}
