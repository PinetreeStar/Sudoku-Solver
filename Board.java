package v1;

import java.util.Arrays;

public class Board {
	
	private int[] primes = {2,3,5,7,11,13,17,19,23};
	private int[][] puz = new int[9][9];
	private boolean valid;
	private boolean solved;

	public Board(int[] nums, boolean debugFlag) {
		if (nums.length != 81) {
			this.errorCode("Invalid puzzle size", debugFlag);
			this.valid = false;
			return;
		}
		for (int i = 0; i < 81; i ++) {
			if ((nums[i] < 0) || (nums[i] > 9)) {
				this.errorCode("Invalid input at index " + (i+1), debugFlag);
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
		this.valid = this.validate(debugFlag);
		this.solved = this.check(debugFlag);
	}
	
	public void status() {
		if (this.validate(true)) {
			this.valid = true;
			System.out.println("This is a valid puzzle");
		}else {
			this.valid = false;
		}
		if (this.check(true)) {
			this.solved = true;
			System.out.println("This puzzle is solved");
		}else {
			this.solved = false;
		}
	}
	
	private boolean validate(boolean debugFlag) {
		boolean once;
		for (int r = 0; r < 9; r ++) {
			for (int p : primes) {
				once = false;
				for (int c = 0; c < 9; c ++) {
					if (p == puz[r][c]) {
						if (!once) {
							once = true;
						}else {
							this.errorCode("More than one instance of a number in row " + (r+1), debugFlag);
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
							this.errorCode("More than one instance of a number in column " + (c+1), debugFlag);
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
									this.errorCode("More than one instance of a number in box " + (r+2) + "," + (c+2), debugFlag);
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
	
	private boolean check(boolean debugFlag) {
		for (int p : primes) {
			for (int r = 0; r < 9; r ++) {
				for (int c = 0; c < 9; c ++) {
					if (p == this.puz[r][c]) {
						continue;
					}
					if (c == 8) {
						this.errorCode("Currently unsolved", debugFlag);
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
						this.errorCode("Currently unsolved", debugFlag);
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
								this.errorCode("Currently unsolved", debugFlag);
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
	
	//All functions beneath this comment can be moved to a Solve class. If they are moved, something will have to be done about/with errorCode()
	
	private int[] findFactors(int num) {
		int[] ret = new int[9];
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
			if (Arrays.binarySearch(findFactors(this.puz[r][c]), p) >= 0) {
				this.puz[r][c] /= p;
				ret = true;
			}
		}
		return ret;
	}
	
	private boolean divideColumn(int p, int c) {
		boolean ret = false;
		for (int r = 0; r < 9; r ++) {
			if (Arrays.binarySearch(findFactors(this.puz[r][c]), p) >= 0) {
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
				if (Arrays.binarySearch(findFactors(this.puz[rr][cc]), p) >= 0) {
					this.puz[rr][cc] /= p;
					ret = true;
				}
			}
		}
		return ret;
	}
	
	private void errorCode(String message, boolean debugFlag) {
		if (debugFlag) {
			System.out.println(message);
		}
	}
	
	public void solve(boolean debugFlag) {
		if (this.solved) {
			this.errorCode("This puzzle is already solved", debugFlag);
			return;
		}
		if (!valid) {
			this.errorCode("Cannot solve an invalid puzzle", debugFlag);
			return;
		}
		this.simpleSolver(debugFlag);
		boolean changes;
		do {
			changes = false;
			if (this.rowSolver(debugFlag)) {
				changes = true;
			}
			if (this.columnSolver(debugFlag)) {
				changes = true;
			}
			if (this.boxSolver(debugFlag)) {
				changes = true;
			}
			if (!changes) {
				if (this.plainRowSolver(debugFlag)) {
					changes = true;
				}
				if (this.plainColumnSolver(debugFlag)) {
					changes = true;
				}
				if (this.plainBoxSolver(debugFlag)) {
					changes = true;
				}
			}
			if (!changes) {
				if (this.pointerRowSolver(debugFlag)) {
					changes = true;
				}
				if (this.pointerColumnSolver(debugFlag)) {
					changes = true;
				}
				if (this.pointerBoxSolver(debugFlag)) {
					changes = true;
				}
			}
			if (!changes) {
				//Add hidden solvers later, time and understanding permitting
			}
		}while (changes);
		if (this.check(false)) {
			//this.printBoard(debugFlag);
			//End of solve() with solved puzzle
		}
		this.errorCode("This puzzle is currently unsolvable", debugFlag);
	}
	
	private void simpleSolver(boolean debugFlag) {
		boolean changes = false;
		do {
			for (int r = 0; r < 9; r ++) {
				for (int c = 0; c < 9; c ++) {
					int[] factors = this.findFactors(this.puz[r][c]);
					if (factors.length == 0) {
						this.errorCode("Invalid puzzle", debugFlag);
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

	private boolean rowSolver(boolean debugFlag) {
		boolean ret = false;
		int cIndex;
		
		for (int p : this.primes) {
			for (int r = 0; r < 9; r ++) {
				cIndex = -1;				
				for (int c = 0; c < 9; c ++) {
					if (Arrays.binarySearch(this.findFactors(this.puz[r][c]), p) >= 0) {
						if (cIndex == -1) {
							cIndex = c;
						}else {
							cIndex = -2;
						}
					}
				}
				if (cIndex >= 0) {
					ret = true;
					this.puz[r][cIndex] = p;
					this.simpleSolver(debugFlag);
				}
			}
		}
		return ret;
	}
	
	private boolean columnSolver(boolean debugFlag) {
		boolean ret = false;
		int rIndex;
		
		for (int p : this.primes) {
			for (int c = 0; c < 9; c ++) {
				rIndex = -1;
				for (int r = 0; r < 9; r ++) {
					if (Arrays.binarySearch(this.findFactors(this.puz[r][c]), p) >= 0) {
						if (rIndex == -1) {
							rIndex = r;
						}else {
							rIndex = -2;
						}
					}
				}
				if (rIndex >= 0) {
					ret = true;
					this.puz[rIndex][c] = p;
					this.simpleSolver(debugFlag);
				}
			}
		}
		return ret;
	}
	
	private boolean boxSolver(boolean debugFlag) {
		boolean ret = false;
		int rIndex;
		int cIndex;
		
		for (int p : this.primes) {
			for (int r = 0; r < 9; r += 3) {
				for (int c = 0; c < 9; c += 3) {
					rIndex = -1;
					cIndex = -1;
					for (int rr = r; rr < (r+3); rr ++) {
						for (int cc = c; cc < (c+3); cc ++) {
							if (Arrays.binarySearch(this.findFactors(this.puz[rr][cc]), p) >= 0) {
								if (rIndex == -1) {
									rIndex = rr;
									cIndex = cc;
								}else {
									rIndex = -2;
									cIndex = -2;
								}
							}
						}
					}
					if (rIndex >= 0) {
						ret = true;
						this.puz[rIndex][cIndex] = p;
						this.simpleSolver(debugFlag);
					}
				}
			}
		}
		return ret;
	}
	
	private boolean plainRowSolver(boolean debugFlag) {
		boolean ret = false;
		int[] factors;
		int[] cIndices = new int[9];
		int ctr;
		int original;
		
		for (int r = 0; r < 9; r ++) {
			for (int c = 0; c < 9; c ++) {
				factors = this.findFactors(this.puz[r][c]);
				if (factors.length > 1) {
					ctr = 0;
					for (int cc = 0; cc < 9; cc ++) {
						if ((this.puz[r][cc] % this.puz[r][c]) == 0) {
							cIndices[ctr++] = cc;
						}
					}
					ctr --;
					if (factors.length == ctr) {
						ret = true;
						original = this.puz[r][c];
						for (int f : factors) {
							this.divideRow(f, r);
						}
						for (int cc = 0; cc <= ctr; cc ++) {
							this.puz[r][cc] = original;
						}
						this.simpleSolver(debugFlag);
					}
				}
			}
		}
		return ret;
	}
	
	private boolean plainColumnSolver(boolean debugFlag) {
		boolean ret = false;
		int[] factors;
		int[] rIndices = new int[9];
		int ctr;
		int original;
		
		for (int c = 0; c < 9; c ++) {
			for (int r = 0; r < 9; r ++) {
				factors = this.findFactors(this.puz[r][c]);
				if (factors.length > 1) {
					ctr = 0;
					for (int rr = 0; rr < 9; rr ++) {
						if ((this.puz[rr][c] % this.puz[r][c]) == 0) {
							rIndices[ctr++] = rr;
						}
					}
					ctr --;
					if (factors.length == ctr) {
						ret = true;
						original = this.puz[r][c];
						for (int f : factors) {
							this.divideColumn(f,  c);
						}
						for (int rr = 0; rr <= ctr; rr ++) {
							this.puz[rr][c] = original;
						}
						this.simpleSolver(debugFlag);
					}
				}
			}
		}
		return ret;
	}
	
	private boolean plainBoxSolver(boolean debugFlag) {
		boolean ret = false;
		int[] factors;
		int[] rIndices = new int[9];
		int[] cIndices = new int[9];
		int ctr;
		int original;
		
		for (int r = 0; r < 9; r += 3) {
			for (int c = 0; c < 9; c += 3) {
				for (int rr = r; rr < (r+3); rr ++) {
					for (int cc = c; cc < (c+3); cc ++) {
						factors = this.findFactors(this.puz[rr][cc]);
						if (factors.length > 1) {
							ctr = 0;
							for (int rrr = r; rrr < (r+3); rrr ++) {
								for (int ccc = c; ccc < (c+3); ccc ++) {
									if ((this.puz[rrr][ccc] % this.puz[rr][cc]) == 0) {
										rIndices[ctr] = rrr;
										cIndices[ctr++] = ccc;
									}
								}
							}
							ctr --;
							if (factors.length == ctr) {
								ret = true;
								original = this.puz[rr][cc];
								for (int f : factors) {
									this.divideBox(f, r, c);
								}
								for (int rrr = r; rrr < (r+3); rrr ++) {
									for (int ccc = c; ccc < (c+3); ccc ++) {
										this.puz[rrr][ccc] = original;
									}
								}
								this.simpleSolver(debugFlag);
							}
						}
					}
				}
			}
		}
		return ret;
	}
	
	private boolean pointerRowSolver(boolean debugFlag) {
		boolean ret = false;
		int[] cIndices = new int[9];
		int ctr;
		int boxC;
		boolean different;
		
		for (int p : this.primes) {
			for (int r = 0; r < 9; r ++) {
				ctr = 0;
				for (int c = 0; c < 9; c ++) {
					if (Arrays.binarySearch(this.findFactors(this.puz[r][c]), p) >= 0) {
						cIndices[ctr++] = c;
					}
				}
				if (ctr > 1) {
					ret = true;
					boxC = (int) (cIndices[0] / 3);
					different = false;
					for (int i = 0; i < ctr; i ++) {
						if (((int) cIndices[i] / 3) != boxC) {
							different = true;
						}
					}
					if (!different) {
						this.divideBox(p, r, cIndices[0]);
					}
					for (int i = 0; i < ctr; i ++) {
						this.puz[r][cIndices[i]] *= p;
					}
					this.simpleSolver(debugFlag);
				}
			}
		}
		return ret;
	}
	
	private boolean pointerColumnSolver(boolean debugFlag) {
		boolean ret = false;
		int[] rIndices = new int[9];
		int ctr;
		int boxR;
		boolean different;
		
		for (int p : this.primes) {
			for (int c = 0; c < 9; c ++) {
				ctr = 0;
				for (int r = 0; r < 9; r ++) {
					if (Arrays.binarySearch(this.findFactors(this.puz[r][c]), p) >= 0) {
						rIndices[ctr++] = r;
					}
				}
				if (ctr > 1) {
					ret = true;
					boxR = (int) (rIndices[0] / 3);
					different = false;
					for (int i = 0; i < ctr; i ++) {
						if (((int) rIndices[i] / 3) != boxR) {
							different = true;
						}
					}
					if (!different) {
						this.divideBox(p, rIndices[0], c);
					}
					for (int i = 0; i < ctr; i ++) {
						this.puz[rIndices[i]][c] *= p;
					}
					this.simpleSolver(debugFlag);
				}
			}
		}
		return ret;
	}
	
	private boolean pointerBoxSolver(boolean debugFlag) {
		boolean ret = false;
		int[] rIndices = new int[9];
		int[] cIndices = new int[9];
		int ctr;
		int row;
		int col;
		boolean different;
		
		for (int p : this.primes) {
			for (int r = 0; r < 9; r += 3) {
				for (int c = 0; c < 9; c += 3) {
					ctr = 0;
					for (int rr = r; rr < (r+3); rr ++) {
						for (int cc = c; cc < (c+3); cc ++) {
							if (Arrays.binarySearch(this.findFactors(this.puz[rr][cc]), p) >= 0) {
								rIndices[ctr] = rr;
								cIndices[ctr++] = cc;
							}
						}
					}
					if (ctr > 1) {
						ret = true;
						row = rIndices[0];
						different = false;
						for (int rI = 0; rI < ctr; rI ++) {
							if (rIndices[rI] != row) {
								different = true;
							}
						}
						if (!different) {
							this.divideRow(p, row);
						}
						for (int rI = 0; rI < ctr; rI ++) {
							this.puz[row][cIndices[rI]] *= p;
						}
						
						col = cIndices[0];
						different = false;
						for (int cI = 0; cI < ctr; cI ++) {
							if (cIndices[cI] != col) {
								different = true;
							}
						}
						if (!different) {
							this.divideColumn(p, col);
						}
						for (int cI = 0; cI < ctr; cI ++) {
							this.puz[rIndices[cI]][col] *= p;
						}
						this.simpleSolver(debugFlag);
					}
				}
			}
		}
		return ret;
	}
	
}
