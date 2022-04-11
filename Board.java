package v1;

import java.util.Arrays;

public class Board {
	
	private int[] primes = {2,3,5,7,11,13,17,19,23};
	protected int[][] puz = new int[9][9];
	protected boolean valid = true;
	protected boolean solved = false;

	public Board(int[] nums, boolean debugFlag) {
		if (nums.length != 81) {
			System.out.println("Invalid puzzle size");
			this.valid = false;
			return;
		}
		for (int i = 0; i < 81; i ++) {
			if ((nums[i] < 0) || (nums[i] > 9)) {
				System.out.println("Invalid input at index " + (i+1));
				this.valid = false;
				return;
			}else if (nums[i] == 0) {
				puz[(int) i/9][i%9] = 223092870;
			}else if (nums[i] == 1) {
				puz[(int) i/9][i%9] = 2;
			}else if (nums[i] == 2) {
				puz[(int) i/9][i%9] = 3;
			}else if (nums[i] == 3) {
				puz[(int) i/9][i%9] = 5;
			}else if (nums[i] == 4) {
				puz[(int) i/9][i%9] = 7;
			}else if (nums[i] == 5) {
				puz[(int) i/9][i%9] = 11;
			}else if (nums[i] == 6) {
				puz[(int) i/9][i%9] = 13;
			}else if (nums[i] == 7) {
				puz[(int) i/9][i%9] = 17;
			}else if (nums[i] == 8) {
				puz[(int) i/9][i%9] = 19;
			}else if (nums[i] == 9) {
				puz[(int) i/9][i%9] = 23;
			}
		}
		if (debugFlag) {
			this.status();
		}
	}
	
	public String toString() {
		String ret = "";
		for (int r = 0; r < 9; r ++) {
			for (int c = 0; c < 9; c ++) {
				
				int val = Arrays.binarySearch(this.primes, this.puz[r][c]) + 1;
				if (val > 0) {
					ret += val + " ";
				}else {
					ret += "_ ";
				}
				
				//ret += (this.puz[r][c] + " ");
			}
			ret += '\n';
		}
		return ret;
	}
	
	public void status() {
		if (this.validate()) {
			this.valid = true;
			System.out.println("This is a valid puzzle");
		}else {
			this.valid = false;
		}
		if (this.check()) {
			this.solved = true;
			System.out.println("This puzzle is solved");
		}else {
			System.out.println("This puzzle is currently unsolved");
			this.solved = false;
		}
	}
	
	private boolean validate() {
		if (!this.valid) {
			return false;
		}
		boolean once;
		for (int r = 0; r < 9; r ++) {
			for (int p : primes) {
				once = false;
				for (int c = 0; c < 9; c ++) {
					if (p == puz[r][c]) {
						if (!once) {
							once = true;
						}else {
							System.out.println("More than one instance of a number in row " + (r+1));
							return false;
						}
					}
				}
			}
		}
		for (int c = 0; c < 9; c ++) {
			for (int p : primes) {
				once = false;
				for (int r = 0; r < 9; r ++) {
					if (p == puz[r][c]) {
						if (!once) {
							once = true;
						}else {
							System.out.println("More than one instance of a number in column " + (c+1));
							return false;
						}
					}
				}
			}
		}
		for (int r = 0; r < 9; r += 3) {
			for (int c = 0; c < 9; c += 3){
				for (int p : primes) {
					once = false;
					for (int rr = r; rr < (r+3); rr ++) {
						for (int cc = c; cc < (c+3); cc ++) {
							if (p == puz[rr][cc]) {
								if (!once) {
									once = true;
								}else {
									System.out.println("More than one instance of a number in box " + (r+2) + "," + (c+2));
									return false;
								}
							}
						}
					}
				}
			}
		}
		return true;
	}
	
	protected boolean check() {
		for (int p : primes) {
			for (int r = 0; r < 9; r ++) {
				for (int c = 0; c < 9; c ++) {
					if (p == this.puz[r][c]) {
						break;
					}
					if (c == 8) {
						return false;
					}
				}
			}
			for (int c = 0; c < 9; c ++) {
				for (int r = 0; r < 9; r ++) {
					if (p == this.puz[r][c]) {
						break;
					}
					if (r == 8) {
						return false;
					}
				}
			}
			for (int r = 0; r < 9; r += 3) {
				for (int c = 0; c < 9; c += 3) {
					boolean bypass = false;
					for (int rr = r; rr < (r+3); rr ++) {
						for (int cc = c; cc < (c+3); cc ++) {
							if (p == this.puz[rr][cc]) {
								bypass = true;
								break;
							}
							if ((cc == (c+3)) && (rr == (r+3))) {
								return false;
							}
						}
						if (bypass) {
							break;
						}
					}
				}
			}
		}
		return true;
	}
	
}
