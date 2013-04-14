package com.blogspot.remisthoughts.compiletoasm;

import static com.blogspot.remisthoughts.compiletoasm.Compiler.indexOf;
import static org.gnu.glpk.GLPK._glp_lpx_get_mat_row;
import static org.gnu.glpk.GLPK.doubleArray_getitem;
import static org.gnu.glpk.GLPK.glp_mip_col_val;
import static org.gnu.glpk.GLPK.glp_mip_row_val;
import static org.gnu.glpk.GLPK.intArray_getitem;
import static org.gnu.glpk.GLPK.new_doubleArray;
import static org.gnu.glpk.GLPK.new_intArray;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.gnu.glpk.SWIGTYPE_p_double;
import org.gnu.glpk.SWIGTYPE_p_int;
import org.gnu.glpk.glp_prob;

import com.blogspot.remisthoughts.compiletoasm.Compiler;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ColumnKey;
import com.blogspot.remisthoughts.compiletoasm.Compiler.ControlFlowGraph;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Instruction;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Register;
import com.blogspot.remisthoughts.compiletoasm.Compiler.RowKey;
import com.blogspot.remisthoughts.compiletoasm.Compiler.Variable;
import com.google.common.base.Joiner;


class DebugUtils {

	/**
	 * Assumes var is only in one register at once...it might not be if the
	 * constraints are incorrect!
	 */
	 static void printVariableLife(glp_prob lpProblem, ControlFlowGraph cfg, List<Instruction> code, Variable variable) {
		System.out.println(variable.name);
		int v = indexOf(cfg.variables, variable);
		for (int i = 0; i < code.size(); ++i) {
			int n = cfg.prevNode[i];
			try {
				Register r = Compiler.whereAmI(lpProblem, cfg, v, n);
				System.out.printf("%02d: %s\n", i, r);
			} catch (IllegalStateException e) {
				System.out.printf("%02d: NONE\n", i);
			}
		}
	}

	 static void printRegisterLife(glp_prob lpProblem, ControlFlowGraph cfg, List<Instruction> code, Register r) {
		System.out.println(r.name());
		for (int i = 0; i < code.size(); ++i) {
			int n = cfg.prevNode[i];
			List<Variable> vars = new ArrayList<Variable>(2);
			for (int v = 0; v < cfg.variables.length; ++v) {
				try {
					if (Compiler.whereAmI(lpProblem, cfg, v, n) == r) {
						vars.add(cfg.variables[v]);
					}
				} catch (IllegalStateException e) {
					// not in any register
				}
			}
			System.out.printf("%02d: %s\n", i, Joiner.on(',').join(vars));
		}
	}

	 static void printShouldBeSwitches(glp_prob lpProblem, ControlFlowGraph cfg, List<Instruction> code) {
		System.out.println("should-be-switches");
		for (int i = 0; i < code.size(); ++i) {
			int n = cfg.prevNode[i];
			for (int prevN : cfg.nextNodes.inverse().get(n)) {
				for (int v = 0; v < cfg.variables.length; ++v) {
					for (Register r : Register.ASSIGNABLE) {
						boolean rPrevN;
						try {
							rPrevN = Compiler.whereAmI(lpProblem, cfg, v, prevN) == r;
						} catch (IllegalStateException e) {
							rPrevN = false; // it wasn't in any register
						}
						boolean rN;
						try {
							rN = Compiler.whereAmI(lpProblem, cfg, v, n) == r;
						} catch (IllegalStateException e) {
							rN = false; // it wasn't in any register
						}
						boolean AssN = cfg.needsAssigning(v, n, r);
						boolean AssPrevN = cfg.needsAssigning(v, prevN, r);
						if (AssN && AssPrevN && rN && !rPrevN) {
							System.out.printf("%02d: %s just-arrived-at %s\n", i, cfg.variables[v], r);
						} else if (!rPrevN && rN) {
							System.out.printf("%02d: %s new? %s\n", i, cfg.variables[v], r);
						}
					}
				}
			}
		}
	}

	 static void printColumns(ControlFlowGraph cfg, glp_prob problem, int n) {
		for (int i = 0; i < cfg.columns.size(); ++i) {
			ColumnKey column = cfg.columns.get(i);
			if (column.n == n) {
				System.out.printf("%3d %s p:%.0f\n", i, column, glp_mip_col_val(problem, i + 1));
			}
		}
	}

	 static void printRows(ControlFlowGraph cfg, glp_prob problem, int n) {
		for (int i = 0; i < cfg.rows.size(); ++i) {
			RowKey row = cfg.rows.get(i);
			if (row.n == n) {
				System.out.printf("%3d %s p:%.0f\n", i, row, glp_mip_row_val(problem, i + 1));
			}
		}
	}

	 static void printRow(ControlFlowGraph cfg, glp_prob problem, RowKey key) {
		int row = Collections.binarySearch(cfg.rows, key);
		assertTrue(row >= 0);

		SWIGTYPE_p_int columns = new_intArray(cfg.columns.size());
		SWIGTYPE_p_double values = new_doubleArray(cfg.columns.size());
		_glp_lpx_get_mat_row(problem, row + 1, columns, values);
		for (int c = 0; c < cfg.columns.size(); ++c) {
			int column = intArray_getitem(columns, c);
			if (column > 0) {
				System.out.printf("%s %.0f\n", cfg.columns.get(column - 1), doubleArray_getitem(values, c));
			}
		}
	}

	 static int numNodesLiveAtBefore(ControlFlowGraph cfg, int v, int n) {
		int ret = 0;
		for (int prev : cfg.nextNodes.inverse().get(n)) {
			if (cfg.isLiveAt(v, prev)) {
				++ret;
			}
		}
		return ret;
	}

}
