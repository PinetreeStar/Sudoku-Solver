package v1;

import java.util.Arrays;

public class Board {
	
	private int[] primes = {2,3,5,7,11,13,17,19,23};
	private int[][] puz = new int[9][9];
	private boolean valid;
	private boolean solved;

	public Board(int[] nums, boolean print) {
		if (nums.length != 81) {
			return;
		}
		for (int i = 0; i < 81; i ++) {
			if ((nums[i] < 0) || (nums[i] > 9)) {
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
		this.valid = this.validate(print);
		this.solved = this.check(print);
	}
	
	public void status() {
		if (this.valid) {
			System.out.println("This is a valid puzzle");
		}else {
			System.out.println("This is an invalid puzzle");
		}
		if (this.solved) {
			System.out.println("This puzzle is solved");
		}else {
			System.out.println("This puzzle is not solved");
		}
	}
	
	private int[] findFactors(int num) {
		int[] ret = {0,0,0,0,0,0,0,0,0};
		int ctr = 0;
		for (int p : this.primes) {
			if ((num % p) == 0) {
				ret[ctr++] = p;
				num /= p;
			}
		}
		return Arrays.copyOf(ret, ctr);
	}
	
	private boolean divideRow(int p, int r) {
		boolean ret = false;
		for (int c = 0; c < 9; c ++) {
			if (Arrays.binarySearch(findFactors(this.puz[r][c]), p) < 0) {
				this.puz[r][c] /= p;
				ret = true;
			}
		}
		return ret;
	}
	
	private boolean divideColumn(int p, int c) {
		boolean ret = false;
		for (int r = 0; r < 9; r ++) {
			if (Arrays.binarySearch(findFactors(this.puz[r][c]), p) < 0) {
				this.puz[r][c] /= p;
				ret = true;
			}
		}
		return ret;
	}
	
	private boolean divideBox(int p, int r, int c) {
		boolean ret = false;
		r = ((int) r / 3);
		c = ((int) c / 3);
		for (int rr = r; rr < (r+3); rr ++) {
			for (int cc = c; cc < (c+3); cc ++) {
				if (Arrays.binarySearch(findFactors(this.puz[rr][cc]), p) < 0) {
					this.puz[rr][cc] /= p;
					ret = true;
				}
			}
		}
		return ret;
	}
	
	private boolean validate(boolean print) {
		for (int r = 0; r < 9; r ++) {
			for (int c = 0; c < 9; c ++) {
				if (puz[r][c] == 1) {
					this.errorCode("Invalid puzzle", print);
					return false;
				}
			}
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
							this.errorCode("More than one instance of a number in row " + (r+1), print);
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
							this.errorCode("More than one instance of a number in column " + (c+1), print);
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
									this.errorCode("More than one instance of a number in box " + (r+2) + "," + (c+2), print);
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
	
	private void errorCode(String message, boolean print) {
		if (print) {
			System.out.println(message);
		}
	}
	
	public void solve(boolean print) {
		if (this.solved) {
			this.errorCode("This puzzle is already solved", print);
			return;
		}
		if (!valid) {
			this.errorCode("Cannot solve an invalid puzzle", print);
			return;
		}
		this.simpleSolver(print);
		boolean changes;
		do {
			changes = false;
			if (this.rowSolver(print)) {
				changes = true;
			}
			if (this.columnSolver(print)) {
				changes = true;
			}
			if (this.columnSolver(print)) {
				changes = true;
			}
			if (!changes) {
				if (this.plainRowSolver(print)) {
					changes = true;
				}
				if (this.plainColumnSolver(print)) {
					changes = true;
				}
				if (this.plainBoxSolver(print)) {
					changes = true;
				}
			}
			if (!changes) {
				if (this.pointerRowSolver(print)) {
					changes = true;
				}
				if (this.pointerColumnSolver(print)) {
					changes = true;
				}
				if (this.pointerBoxSolver(print)) {
					changes = true;
				}
			}
			if (!changes) {
				//Add hidden solvers later, time and understanding permitting
			}
		}while (changes);
		this.check(print);
	}
	
	private void simpleSolver(boolean print) {
		boolean changes = false;
		do {
			for (int r = 0; r < 9; r ++) {
				for (int c = 0; c < 9; c ++) {
					int[] factors = this.findFactors(this.puz[r][c]);
					if (factors.length == 0) {
						this.errorCode("Invalid puzzle", print);
					}else if (factors.length == 1){
						if (this.divideRow(factors[0], r)) {
							changes = true;
						}
						if (this.divideColumn(factors[0], c)) {
							changes = true;
						}
						if (this.divideBox(factors[0], r, c)) {
							changes = true;
						}
					}
				}
			}
		}while (changes);
	}
	
	private boolean check(boolean print) {
		for (int p : primes) {
			for (int r = 0; r < 9; r ++) {
				for (int c = 0; c < 9; c ++) {
					if (p == this.puz[r][c]) {
						continue;
					}
					if (c == 8) {
						this.errorCode("Currently unsolved", print);
						return false;
					}
				}
			}
			for (int c = 0; c < 9; c ++) {
				for (int r = 0; r < 9; r ++) {
					if (p == this.puz[r][c]) {
						continue;
					}
					if (r == 8) {
						this.errorCode("Currently unsolved", print);
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
								this.errorCode("Currently unsolved", print);
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
	}
	
}
