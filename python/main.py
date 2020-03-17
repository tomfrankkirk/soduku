# Soduku solvers

import numpy as np 
import itertools
import copy
import functools 
import operator

FULL_SET = set(range(1,10))

class Soduku(object):

    def __init__(self, arr):
        self.board = arr
        self.protected = (arr > 0)

    def row(self, idx): 
        return set(self.board[idx,:])

    def col(self, idx):
        return set(self.board[:,idx])

    def block(self, row, col):
        R = 3 * (row // 3)
        C = 3 * (col // 3)
        return set(self.board[R:R+3, C:C+3].reshape(-1))

    def possible_moves(self):
        moves = np.empty((9,9), dtype=object)
        for x,y in itertools.product(range(9), range(9)):
            if self.board[x,y] == 0:
                r = FULL_SET - self.row(x)
                c = FULL_SET - self.col(y)
                b = FULL_SET - self.block(x,y)
                moves[x,y] = tuple(r & c & b)
            else: 
                moves[x,y] = tuple()

        return moves.flatten()

    @property
    def assigned(self):
        return (self.board > 0)

    @property
    def complete(self):
        return self.assigned.sum() == 81 

    def __str__(self):
        return self.board.__str__()

    def __eq__(self, other):
        return (self.board == other.board).all()

    def violations(self): 
        v = np.zeros(self.board.shape, dtype=bool)
        for x,y in itertools.product(range(9), range(9)):
            if not self.protected[x,y]:
                r = self.row(x)
                c = self.col(y)
                b = self.block(x,y)
                v[x,y] = not all([ r == FULL_SET, c == FULL_SET, b == FULL_SET ])

        return v 

    def fitness(self):
        repeats = 0 
        missing = 0 
        for r in range(9):
            missing += len(FULL_SET - self.row(r))
            repeats += np.sum(np.bincount(self.board[r,:]) > 1)

        for c in range(9):
            missing += len(FULL_SET - self.col(c))
            repeats += np.sum(np.bincount(self.board[:,c]) > 1)

        for r,c in itertools.product([0,3,6], [0,3,6]):
            missing += len(FULL_SET - self.block(r,c))
            R = 3*(r // 3)
            C = 3*(c // 3)
            repeats += np.sum(np.bincount(self.board[R:R+3,C:C+3].flatten()) > 1)

        return missing + repeats

    def correct(self):
        return (self.violations().sum() == 0) 

    def violations_inds(self):
        return np.flatnonzero(self.violations())    

    def swap(self, v1, v2):
        v1 = np.unravel_index(v1, (9,9))
        v2 = np.unravel_index(v2, (9,9))
        tmp = self.board[v2]
        self.board[v2] = self.board[v1]
        self.board[v1] = tmp  

    def print_violations(self):
        print(self.violations_inds().astype(np.int16))

def solve(puzzle):

    if puzzle.complete: 
        assert puzzle.correct()
        return puzzle 

    else: 
        moves = puzzle.possible_moves()
        n_moves = np.array([ len(m) for m in moves ])
        one_moves = (n_moves == 1)
        
        if one_moves.any():
            next_loc = np.where(one_moves)[0][0]
            next_move = moves[next_loc][0]
            puzzle.board[ np.unravel_index(next_loc, (9,9)) ] = next_move 
            return solve(puzzle)
        
        else: 
            raise RuntimeError("Puzzle not determinate")


def backtrace(puzzle): 

    if puzzle.complete: 
        assert puzzle.correct()
        return puzzle 

    else: 

        for x,y in itertools.product(range(9), range(9)):
            if puzzle.board[x,y] == 0: 
                r = FULL_SET - puzzle.row(x)
                c = FULL_SET - puzzle.col(y)
                b = FULL_SET - puzzle.block(x,y)
                allowed = sorted(tuple(r & c & b))

                for a in allowed: 
                    puzzle.board[x,y] = a 
                    next_soln = backtrace(puzzle)
                    if next_soln.complete:
                        return next_soln
                    else: 
                        puzzle.board[x,y] = 0 
                        continue

                return puzzle
                
# def stochastic(puzzle):

#     def istaboo(puzzle, taboo_list):
#         return any([ p == puzzle for p in taboo_list ])

#     def initialise(puzzle):
#         puzzle.board[~puzzle.protected] = 0 
#         counts = np.bincount(puzzle.board.flatten(), minlength=10)
#         missing = np.repeat(np.arange(1,10), (9 - counts[1:])).tolist()
#         np.random.shuffle(missing)
#         missing_inds = puzzle.violations_inds() 
#         assert len(missing) == missing_inds.size
        
#         for ind in missing_inds:
#             x,y = np.unravel_index(ind, (9,9))
#             b = FULL_SET - puzzle.block(x,y)
#             for val in missing:
#                 if val in b: 
#                     puzzle.board[x,y] = val 
#                     missing.remove(val)
#                     break 

#         assert np.all(puzzle.board > 0)
#         return puzzle 

#     def shuffle_violations(puzzle):
#         v_inds = np.unravel_index(puzzle.violations_inds(), (9,9))
#         vals = puzzle.board[v_inds]
#         np.random.shuffle(vals)
#         puzzle.board[v_inds] = vals 
#         return puzzle 


#     puzzle = initialise(puzzle)
#     taboo = [] 
#     best_cost = puzzle.cost()

#     while not puzzle.correct(): 
        
#         vlns_inds = puzzle.violations_inds()
#         root = vlns_inds[0]
#         rx,ry = np.unravel_index(root, (9,9))
#         block = puzzle.block(rx,ry) - {puzzle.board[rx,ry]}
#         permitted_swaps = [] 
#         permitted_costs = [] 
#         for other in vlns_inds[1:]:
#             ox,oy = np.unravel_index(other, (9,9))
#             if puzzle.board[ox,oy] not in block:
#                 puzzle.swap(root, other)
#                 if not istaboo(puzzle, taboo):
#                     permitted_swaps.append(other)
#                     permitted_costs.append(puzzle.cost())
#                 puzzle.swap(other, root)

#         # Pick the best slns 
#         # if they are better than current they become taboo 
#         if permitted_costs:
#             best_idx = np.argmin(permitted_costs)
#             best_swap = permitted_swaps[best_idx]
#             puzzle.swap(root, best_swap)
#             taboo.append(copy.deepcopy(puzzle))
#             if permitted_costs[best_idx] <= best_cost:
#                 best_cost = permitted_costs[best_idx]
 
#         if (len(taboo) > 30) and np.all(permitted_costs == best_cost): 
#             puzzle = shuffle_violations(puzzle)
#             taboo = [] 


#     # initialise randomly without violating constraint
#     # when calculating neighbourhood of solutions, cannot 
#     # only consider swaps between violations - it could 
#     # be that a non-violating assignment that was already 
#     # performed was incorrect, in which case we are heading
#     # for a local minima 

#     assert puzzle.correct()
#     return puzzle


def csp_ac3(puzzle):

    def row_inds(idx):
        r,_ = np.unravel_index(idx, (9,9))
        inds = np.ravel_multi_index((9 * [r], range(9)), (9,9)).tolist()
        return set(inds) - {idx} 

    def col_inds(idx): 
        _,c = np.unravel_index(idx, (9,9))
        inds = np.ravel_multi_index((range(9), 9 * [c]), (9,9)).tolist()
        return set(inds) - {idx} 

    def block_inds(idx):
        r,c = np.unravel_index(idx, (9,9))
        R = 3 * (r // 3)
        C = 3 * (c // 3)
        coords = np.array(list(itertools.product(range(R,R+3), range(C,C+3))), dtype=np.int32)
        inds = np.ravel_multi_index(coords.T, (9,9)).tolist()
        return set(inds) - {idx} 

    def neighbours(idx):
        return (row_inds(idx) | col_inds(idx) | block_inds(idx))

    def arc_reduce(x, y):
        update = False
        for vx in domains[x]:
            y_permitted = [ constraint(vx, vy) for vy in domains[y] ]
            if not any(y_permitted):
                domains[x] = domains[x] - {vx}
                update = True 
        return update

    def constraint(vx, vy):
        return vx != vy

    initial = puzzle.board.flatten()
    domains = 81 * [None]
    for idx, val in enumerate(initial):
        if val > 0: 
            domains[idx] = {val}
        else: 
            domains[idx] = set(range(1,10))

    to_solve = [] 
    for idx in range(81):

        [ to_solve.append((idx, n)) for n in neighbours(idx) ]

        while len(to_solve):
            src, dest = to_solve.pop(0)
            if arc_reduce(src, dest): 
                if len(domains[src]) == 0: 
                    raise RuntimeError("Empty domain")
                else: 
                    touched_nbrs = neighbours(src) 
                    [ to_solve.append((n, src)) for n in touched_nbrs if n != dest ]

    arr = np.array([ list(d)[0] for d in domains ]).reshape(9,9)
    s = Soduku(arr)
    assert s.correct()
    return s 

def genetic(puzzle):


    def score(candidate):
        t = copy.deepcopy(template)
        vals = t.board.flatten()
        vals[vals == 0] = candidate
        t.board = vals.reshape(9,9)
        assert t.complete
        return t.fitness()

    template = copy.copy(puzzle)
    population = [] 
    for p in range(50):
        cand = []
        for i in np.flatnonzero(puzzle.board == 0):
            r,c = np.unravel_index(i, (9,9))
            permitted = list(FULL_SET - (puzzle.row(r) | puzzle.col(c) | puzzle.block(r,c)))
            idx = np.random.randint(0, len(permitted))
            cand.append(permitted[idx])
        population.append(cand)

    generations = 10 
    retain_frac = 0.5
    population = np.array(population)
    for gen in range(generations):
        ranking = np.argsort([ score(population[p,:]) for p in range(population.shape[0]) ])
        ranking = ranking[0:int(retain_frac * population.shape[0])]


if __name__ == "__main__":

    ve = np.array([ 
        [3, 0, 5, 6, 1, 7, 2, 9, 4],
        [7, 9, 4, 3, 2, 5, 6, 0, 8], 
        [1, 2, 6, 0, 9, 4, 0, 5, 7],
        [8, 1, 9, 4, 7, 0, 5, 6, 3],
        [0, 0, 7, 9, 3, 1, 8, 0, 2], 
        [4, 3, 2, 0, 8, 6, 0, 7, 9], 
        [0, 6, 3, 7, 5, 8, 4, 2, 1], 
        [5, 7, 1, 2, 4, 0, 0, 8, 6],
        [2, 4, 8, 1, 0, 9, 0, 3, 5]], dtype=np.int16) 
    
    e = np.array([ 
        [0, 0, 5, 6, 1, 0, 0, 9, 4],
        [7, 0, 0, 3, 0, 5, 0, 0, 0], 
        [0, 0, 0, 0, 9, 0, 0, 5, 7],
        [8, 1, 0, 4, 0, 0, 5, 0, 0],
        [6, 0, 7, 9, 0, 1, 8, 0, 2], 
        [0, 0, 2, 0, 0, 6, 0, 7, 9], 
        [9, 6, 0, 0, 5, 0, 0, 0, 0], 
        [0, 0, 0, 2, 0, 3, 0, 0, 6],
        [2, 4, 0, 0, 6, 9, 7, 0, 0]], dtype=np.int16) 

    h = np.array([
        [0, 2, 0, 6, 0, 8, 0, 0, 0], 
        [5, 8, 0, 0, 0, 9, 7, 0, 0], 
        [0, 0, 0, 0, 4, 0, 0, 0, 0], 
        [3, 7, 0, 0, 0, 0, 5, 0, 0], 
        [6, 0, 0, 0, 0, 0, 0, 0, 4], 
        [0, 0, 8, 0, 0, 0, 0, 1, 3],
        [0, 0, 0, 0, 2, 0, 0, 0, 0], 
        [0, 0, 9, 8, 0, 0, 0, 3, 6], 
        [0, 0, 0, 3, 0, 6, 0, 9, 0]], dtype=np.int16)
        

    p = Soduku(e) 
    # print(solve(p))
    print(genetic(p))

    