include("../src/dancinglinks.jl")
using .DancingLinks
using BenchmarkTools

function sudoku2dl(fill::AbstractMatrix{<:Integer})
    ncells = size(fill)
    side = ncells[1]
    side == ncells[2] || throw(ArgumentError("Invalid matrix size $ncells"))
    blockside = isqrt(side)
    blockside^2 == side || throw(ArgumentError("Side $side is not a full square"))
    constr = Base.fill(true, (side, side, 4))
    for col in 1:side, row in 1:side
        num = fill[row,col]
        if num in 1:side
            block = col - (col - 1) % blockside + (row - 1) ÷ blockside
            constr[row, num, 1] = false
            constr[col, num, 2] = false
            constr[block, num, 3] = false
            constr[row, col, 4] = false
        end
    end
    idconstr = sizehint!(Tuple{Symbol, Int, Int}[], 4 * prod(ncells))
    append!(idconstr, (:row, row, num) for row in 1:side, num in 1:side if constr[row,num,1])
    append!(idconstr, (:col, col, num) for col in 1:side, num in 1:side if constr[col,num,2])
    append!(idconstr, (:block, block, num) for block in 1:side, num in 1:side if constr[block,num,3])
    append!(idconstr, (:fill, row, col) for row in 1:side, col in 1:side if constr[row,col,4])
    dlmatr = LinkMatrix(idconstr)
    for row in 1:side, col in 1:side, num in 1:side
        block = col - (col - 1) % blockside + (row - 1) ÷ blockside
        if constr[row,num,1] && constr[col,num,2] && constr[block,num,3] && constr[row,col,4]
            col_ids = (:row, row, num), (:col, col, num), (:block, block, num), (:fill, row, col)
            insert_row!(dlmatr, col_ids)
        end
    end
    return dlmatr
end

function sol2matr(dlsol, fieldsize)
    ans = zeros(Int, fieldsize)
    for dl in dlsol
        indval = row2indval(dl)
        ans[indval.ind] = indval.val
    end
    return ans
end

function row2indval(dlrow)
    row, col, n = 0,0,0
    for col_id in dlrow
        if col_id[1] === :fill
            _, row, col = col_id
        else
            _, _, n = col_id
        end
        all(>(0), (row, col, n)) && break
    end
    return (ind = CartesianIndex(row, col), val = n)
end

function sudoku(fill::AbstractMatrix{<:Integer})
    dlmatr = sudoku2dl(fill)
    cover = algorithm_x!(dlmatr)
    isnothing(cover) && return
    soln = sol2matr(cover, size(fill))
    soln .+= fill
    return soln
end

small_test() = @btime sudoku(
    [
        0 0 0 0 0 0 0 3 9;
        0 0 0 0 1 0 0 0 5;
        0 0 3 0 0 5 8 0 0;
        0 0 8 0 0 9 0 0 6;
        0 7 0 0 2 0 0 0 0;
        1 0 0 4 0 0 0 0 0;
        0 0 9 0 0 8 0 5 0;
        0 2 0 0 0 0 6 0 0;
        4 0 0 7 0 0 0 0 0
    ]
    )

med_test() = @btime sudoku(
    [
         0  0  0  0    0  0  0  0    0  0  0  3    0  0  9 12;
         0  0  0  0    3  0  8  0    9  1  0  4    0  0 14  0;
         5  0  0 10    0  1  0  0   15  0  0  0    0 16  0  0;
         0  0 13 11    0  0  0  0    0  0 14  0    1  8  0  4;
         0  9  3  0    0  0  4  2   14 12  1  0    0 13  0  6;
        13  0  0  2    0  0  0  0    0 11  6  0    0  0  0 16;
        11  0 14  8    0 15  0  0    7  0  2  0    0  0  3  0;
         0 12 16  0    0  0  7  0    0  4 15  8    0  2  0  0;
         0 14 11  0    0  0  6 13    0 15  0  0    0  0  0  8;
         0  0  0  0    0  0  2  0    0  0 13 10    0  0  4  0;
         0  0  1  0    0 12 11  0    0  8  0  0    0  0 15  3;
        16  0  0  0    0  0 15  0    5  0  0  0   12  9  0  0;
         6  0  5  9    0  0  0 14    0  0  0  7   16  0  0 10;
        10  8  0  0    0  0  0  3    4  0 12  5    0  0  0  0;
         0  1  0  0   15 10  0  0    8  0  0 14    2  0  6  0;
         0  0  0  7    0  6  0  0    0  0  0  0    0 11  0  0
    ]
    )

function str2mat(s::AbstractString)
    side = isqrt(length(s))
    mat = Matrix{Int8}(undef, side, side)
    for i in 1:side, j in 1:side
        n = (i-1) * side + j
        mat[i,j] = s[n] - '0'
    end
    return mat
end

mat2str(m) = prod(x->x+'0', m')

large_test() = @time open("sudoku_solns.txt", "w") do iosol
    open("all_17_clue_sudokus.txt") do io
        numstr = readline(io)
        println(iosol, numstr)
        while !eof(io)
            puzstr = readline(io)
            print(iosol, puzstr, ',')
            puzzle = str2mat(puzstr)
            soln = sudoku(puzzle)
            println(iosol, mat2str(soln))
        end
    end
end

using SHA

test_soln_correct() = open("sudoku_solns.txt") do f
    sha2_256(f)
end |> bytes2hex |> ==("0bc8dda364db7b99f389b42383e37b411d9fa022204d124cb3c8959eba252f05") |> println