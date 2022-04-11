package v1;

import java.util.Arrays;

public class Solver {
	
	private static int[] primes = {2,3,5,7,11,13,17,19,23};
	
	private static void errorCode(String message, boolean debugFlag) {
		if (debugFlag) {
			System.out.println(message);
		}
	}
	
	private static int[][] copyBoard(Board b){
		int[][] ret = new int[9][9];
		for (int r = 0; r < 9; r ++) {
			for (int c = 0; c < 9; c ++) {
				ret[r][c] = b.puz[r][c];
			}
		}
		return ret;
	}
	
	public static boolean solve(Board puz, boolean debugFlag) {
		if (puz.solved) {
			errorCode("This puzzle is already solved", debugFlag);
			return false;
		}
		if (!puz.valid) {
			errorCode("Cannot solve an invalid puzzle", debugFlag);
			return false;
		}
		simpleSolver(puz, debugFlag);
		int[][] puzPrime;
		do {
			puzPrime = copyBoard(puz);
			rowSolver(puz, debugFlag);
			columnSolver(puz, debugFlag);
			boxSolver(puz, debugFlag);
			
			if (Arrays.deepEquals(puz.puz, puzPrime)) {
				plainRowSolver(puz, debugFlag);
				plainColumnSolver(puz, debugFlag);
				plainBoxSolver(puz, debugFlag);
			}
			
			if (Arrays.deepEquals(puz.puz, puzPrime)) {
				pointerRowSolver(puz, debugFlag);
				pointerColumnSolver(puz, debugFlag);
				pointerBoxSolver(puz, debugFlag);
			}
		}while (!Arrays.deepEquals(puz.puz, puzPrime));
		if (puz.check()) {
			puz.solved = true;
			errorCode(puz.toString(), debugFlag);
			return true;
		}else {
			puz.solved = false;
			errorCode("This puzzle is currently unsolvable", debugFlag);
			return false;
		}
	}

	private static int[] findFactors(int num) {
		int[] ret = new int[9];
		int ctr = 0;
		for (int p : primes) {
			if ((num % p) == 0) {
				ret[ctr++] = p;
				num /= p;
			}
		}
		return Arrays.copyOf(ret, ctr);
	}
	
	private static boolean divideRow(Board b, int p, int r) {
		boolean guard = false;
		boolean ret = false;
		for (int c = 0; c < 9; c ++) {
			if (Arrays.binarySearch(findFactors(b.puz[r][c]), p) >= 0) {
				b.puz[r][c] /= p;
				if (!guard) {
					guard = true;
				}else {
					ret = true;
				}
			}
		}
		return ret;
	}
	
	private static boolean divideColumn(Board b, int p, int c) {
		boolean guard = false;
		boolean ret = false;
		for (int r = 0; r < 9; r ++) {
			if (Arrays.binarySearch(findFactors(b.puz[r][c]), p) >= 0) {
				b.puz[r][c] /= p;
				if (!guard) {
					guard = true;
				}else {
					ret = true;
				}
			}
		}
		return ret;
	}
	
	private static boolean divideBox(Board b, int p, int r, int c) {
		boolean guard = false;
		boolean ret = false;
		r = (((int) r / 3) * 3);
		c = (((int) c / 3) * 3);
		for (int rr = r; rr < (r+3); rr ++) {
			for (int cc = c; cc < (c+3); cc ++) {
				if (Arrays.binarySearch(findFactors(b.puz[rr][cc]), p) >= 0) {
					b.puz[rr][cc] /= p;
					if (!guard) {
						guard = true;
					}else {
						ret = true;
					}
				}
			}
		}
		return ret;
	}
	
	private static void simpleSolver(Board b, boolean debugFlag) {
		boolean changes;
		do {
			changes = false;
			for (int r = 0; r < 9; r ++) {
				for (int c = 0; c < 9; c ++) {
					int[] factors = findFactors(b.puz[r][c]);
					if (factors.length == 0) {
						errorCode("Invalid puzzle", debugFlag);
						return;
					}else if (factors.length == 1){
						if (divideRow(b, factors[0], r)) {
							changes = true;
						}
						if (divideColumn(b, factors[0], c)) {
							changes = true;
						}
						if (divideBox(b, factors[0], r, c)) {
							changes = true;
						}
						b.puz[r][c] *= factors[0];
					}
				}
			}
		}while (changes);
	}

	private static void rowSolver(Board b, boolean debugFlag) {
		int cIndex;
		
		for (int p : primes) {
			for (int r = 0; r < 9; r ++) {
				cIndex = -1;				
				for (int c = 0; c < 9; c ++) {
					if (Arrays.binarySearch(findFactors(b.puz[r][c]), p) >= 0) {
						if (cIndex == -1) {
							cIndex = c;
						}else {
							cIndex = -2;
						}
					}
				}
				if ((cIndex >= 0) && (b.puz[r][cIndex] != p)) {
					b.puz[r][cIndex] = p;
					simpleSolver(b, debugFlag);
				}
			}
		}
	}
	
	private static void columnSolver(Board b, boolean debugFlag) {
		int rIndex;
		
		for (int p : primes) {
			for (int c = 0; c < 9; c ++) {
				rIndex = -1;
				for (int r = 0; r < 9; r ++) {
					if (Arrays.binarySearch(findFactors(b.puz[r][c]), p) >= 0) {
						if (rIndex == -1) {
							rIndex = r;
						}else {
							rIndex = -2;
						}
					}
				}
				if ((rIndex >= 0) && (b.puz[rIndex][c] != p)) {
					b.puz[rIndex][c] = p;
					simpleSolver(b, debugFlag);
				}
			}
		}
	}
	
	private static void boxSolver(Board b, boolean debugFlag) {
		int rIndex;
		int cIndex;
		
		for (int p : primes) {
			for (int r = 0; r < 9; r += 3) {
				for (int c = 0; c < 9; c += 3) {
					rIndex = -1;
					cIndex = -1;
					for (int rr = r; rr < (r+3); rr ++) {
						for (int cc = c; cc < (c+3); cc ++) {
							if (Arrays.binarySearch(findFactors(b.puz[rr][cc]), p) >= 0) {
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
					if ((rIndex >= 0) && (b.puz[rIndex][cIndex] != p)) {
						b.puz[rIndex][cIndex] = p;
						simpleSolver(b, debugFlag);
					}
				}
			}
		}
	}
	
	private static void plainRowSolver(Board b, boolean debugFlag) {
		int[] factors;
		int[] cIndices = new int[9];
		int ctr;
		int original;
		
		for (int r = 0; r < 9; r ++) {
			for (int c = 0; c < 9; c ++) {
				factors = findFactors(b.puz[r][c]);
				if (factors.length > 1) {
					ctr = 0;
					for (int cc = 0; cc < 9; cc ++) {
						if (b.puz[r][cc] == b.puz[r][c]) {
							cIndices[ctr++] = cc;
						}
					}
					if (factors.length == ctr) {
						original = b.puz[r][c];
						for (int f : factors) {
							if (divideRow(b, f, r)) {
							}
						}
						for (int cI = 0; cI < ctr; cI ++) {
							b.puz[r][cIndices[cI]] = original;
						}
						simpleSolver(b, debugFlag);
					}
				}
			}
		}
	}
	
	private static void plainColumnSolver(Board b, boolean debugFlag) {
		int[] factors;
		int[] rIndices = new int[9];
		int ctr;
		int original;
		
		for (int c = 0; c < 9; c ++) {
			for (int r = 0; r < 9; r ++) {
				factors = findFactors(b.puz[r][c]);
				if (factors.length > 1) {
					ctr = 0;
					for (int rr = 0; rr < 9; rr ++) {
						if (b.puz[rr][c] == b.puz[r][c]) {
							rIndices[ctr++] = rr;
						}
					}
					if (factors.length == ctr) {
						original = b.puz[r][c];
						for (int f : factors) {
							if (divideColumn(b, f,  c)) {
							}
						}
						for (int rI = 0; rI < ctr; rI ++) {
							b.puz[rIndices[rI]][c] = original;
						}
						simpleSolver(b, debugFlag);
					}
				}
			}
		}
	}
	
	private static void plainBoxSolver(Board b, boolean debugFlag) {
		int[] factors;
		int[] rIndices = new int[9];
		int[] cIndices = new int[9];
		int ctr;
		int original;
		
		for (int r = 0; r < 9; r += 3) {
			for (int c = 0; c < 9; c += 3) {
				for (int rr = r; rr < (r+3); rr ++) {
					for (int cc = c; cc < (c+3); cc ++) {
						factors = findFactors(b.puz[rr][cc]);
						if (factors.length > 1) {
							ctr = 0;
							for (int rrr = r; rrr < (r+3); rrr ++) {
								for (int ccc = c; ccc < (c+3); ccc ++) {
									if (b.puz[rrr][ccc] == b.puz[rr][cc]) {
										rIndices[ctr] = rrr;
										cIndices[ctr++] = ccc;
									}
								}
							}
							if (factors.length == ctr) {
								original = b.puz[rr][cc];
								for (int f : factors) {
									if (divideBox(b, f, r, c)) {
									}
								}
								for (int i = 0; i < ctr; i ++) {
									b.puz[rIndices[i]][cIndices[i]] = original;
								}
								simpleSolver(b, debugFlag);
							}
						}
					}
				}
			}
		}
	}
	
	private static void pointerRowSolver(Board b, boolean debugFlag) {
		int[] cIndices = new int[9];
		int ctr;
		int boxC;
		boolean different;
		
		for (int p : primes) {
			for (int r = 0; r < 9; r ++) {
				ctr = 0;
				for (int c = 0; c < 9; c ++) {
					if (Arrays.binarySearch(findFactors(b.puz[r][c]), p) >= 0) {
						cIndices[ctr++] = c;
					}
				}
				if (ctr > 1) {
					boxC = (int) (cIndices[0] / 3);
					different = false;
					for (int i = 0; i < ctr; i ++) {
						if (((int) cIndices[i] / 3) != boxC) {
							different = true;
						}
					}
					if (!different) {
						divideBox(b, p, r, cIndices[0]);
						for (int i = 0; i < ctr; i ++) {
							b.puz[r][cIndices[i]] *= p;
						}
					}
					simpleSolver(b, debugFlag);
				}
			}
		}
	}
	
	private static void pointerColumnSolver(Board b, boolean debugFlag) {
		int[] rIndices = new int[9];
		int ctr;
		int boxR;
		boolean different;
		
		for (int p : primes) {
			for (int c = 0; c < 9; c ++) {
				ctr = 0;
				for (int r = 0; r < 9; r ++) {
					if (Arrays.binarySearch(findFactors(b.puz[r][c]), p) >= 0) {
						rIndices[ctr++] = r;
					}
				}
				if (ctr > 1) {
					boxR = (int) (rIndices[0] / 3);
					different = false;
					for (int i = 0; i < ctr; i ++) {
						if (((int) rIndices[i] / 3) != boxR) {
							different = true;
						}
					}
					if (!different) {
						divideBox(b, p, rIndices[0], c);
						for (int i = 0; i < ctr; i ++) {
							b.puz[rIndices[i]][c] *= p;
						}
					}
					simpleSolver(b, debugFlag);
				}
			}
		}
	}
	
	private static void pointerBoxSolver(Board b, boolean debugFlag) {
		int[] rIndices = new int[9];
		int[] cIndices = new int[9];
		int ctr;
		int row;
		int col;
		boolean different;
		
		for (int p : primes) {
			for (int r = 0; r < 9; r += 3) {
				for (int c = 0; c < 9; c += 3) {
					ctr = 0;
					for (int rr = r; rr < (r+3); rr ++) {
						for (int cc = c; cc < (c+3); cc ++) {
							if (Arrays.binarySearch(findFactors(b.puz[rr][cc]), p) >= 0) {
								rIndices[ctr] = rr;
								cIndices[ctr++] = cc;
							}
						}
					}
					if (ctr > 1) {
						row = rIndices[0];
						different = false;
						for (int rI = 0; rI < ctr; rI ++) {
							if (rIndices[rI] != row) {
								different = true;
							}
						}
						if (!different) {
							divideRow(b, p, row);
							for (int cI = 0; cI < ctr; cI ++) {
								b.puz[row][cIndices[cI]] *= p;
							}
						}
						
						col = cIndices[0];
						different = false;
						for (int cI = 0; cI < ctr; cI ++) {
							if (cIndices[cI] != col) {
								different = true;
							}
						}
						if (!different) {
							divideColumn(b, p, col);
							for (int rI = 0; rI < ctr; rI ++) {
								b.puz[rIndices[rI]][col] *= p;
							}
						}
						simpleSolver(b, debugFlag);
					}
				}
			}
		}
	}
	
}
