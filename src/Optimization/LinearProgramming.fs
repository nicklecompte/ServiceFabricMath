namespace PowerSystemsAnalysis.Math

module LinearProgramming =
    type LinearPolynomial< ^T when ^T: (static member Zero: ^T) and ^T : (static member (+): ^T -> ^T -> ^T) and ^T : (static member (*):  ^T -> ^T -> ^T)> =
        | Constant of ^T
        | Variable of ^T*int // 0-based integer indexing on underlying vertex space
        | Sum of LinearPolynomial< ^T> * LinearPolynomial< ^T>
        with
            member inline this.SubspaceDimension =
                let rec subspaceAccumulator (lp: LinearPolynomial< ^T>) (acc:int) =
                    match lp with
                    | Constant _ -> 0
                    | Variable _ -> 1
                    | Sum (a, b) -> subspaceAccumulator a (subspaceAccumulator b acc)
                subspaceAccumulator this 0

            member inline this.NumberOfVariables =
                let rec numVariableAccumulator (lp: LinearPolynomial< ^T>) (acc:int) =
                    match lp with
                    | Constant _ -> 0
                    | Variable (_,n) -> n
                    | Sum (a,b) -> max (numVariableAccumulator a acc) (numVariableAccumulator b acc)
                numVariableAccumulator this 0
            member inline this.Evaluate (x: ^T array) : ^T =

                let rec evaluateAccumulator (lp: LinearPolynomial< ^T>) (acc: ^T) =
                        match lp with
                            | Constant t -> acc + t// (^T : (static member (+) : ^T -> ^T -> ^T) (acc,t)) 
                            | Variable (t,n) -> acc + t*x.[n] //(^T : (static member (*) : ^T -> ^T -> ^T) (acc,((^T : (static member op_Multiply : ^T * ^T -> ^T) (x.[n],t)))))
                            | Sum(a,b) -> evaluateAccumulator a (evaluateAccumulator b acc)
                evaluateAccumulator this LanguagePrimitives.GenericZero

            static member inline ToMatrixPlusVector (input:seq<LinearPolynomial< ^T>>) : ( ^T[,] * ^T[]) =
                    let rowCount = input |> Seq.length
                    let colCount = (input |> Seq.maxBy(fun a -> a.NumberOfVariables)).NumberOfVariables

                    let mutable constArray = Array.init rowCount (fun _ -> LanguagePrimitives.GenericZero< ^T>)
                    let mutable polArray = Array2D.init rowCount colCount (fun _ _ -> LanguagePrimitives.GenericZero< ^T>)
                    input
                    |> Seq.iteri(fun index linearPolynomial ->
                                        let rec assignCoefficientToArray lp =
                                            match lp with
                                            | Constant t -> constArray.[index] <- t
                                            | Variable (t,n) -> polArray.[index,n-1] <- t
                                            | Sum (a,b) -> assignCoefficientToArray (a); assignCoefficientToArray (b)
                                        assignCoefficientToArray linearPolynomial)
                    (polArray,constArray)
            // static member inline ToSeqLinearPolynomialFromMatrixAndVector (coefficientMatrix: ^T[,]) (rhsVector: ^T[]) =
            //     let imageDimension = rhsVector.Length
            //     if coefficientMatrix |> Array2D.length1 <> imageDimension then invalidArg "rhsVector" "rhsVector and coefficientMarix dimension mismatch"
            //     let coImageDimension = coefficientMatrix |> Array2D.length2
            //     let rec BuildMonomialFromSeqT (input: ^T list) (acc:int) (builder: LinearPolynomial< ^T>) =
            //         match input with
            //         | [] -> builder
            //         | x:xs -> BuildMonomialFromSeqT xs (acc+1) Sum(builder,Variable(x,acc))

//                for i in 1..imageDimension do
                    //./

    type ProblemSense = 
        | LessThan
        | LessThanOrEqualTo
        | Equals
        | GreaterThanOrEqualTo
        | GreaterThan

    /// min objectiveFunction*x subject to ((fst constraints_nonconstanstant) x (snd constraints) fst constaints_constant)
     type GeneralLinearProgram< ^T when ^T: (static member Zero: ^T) and ^T : (static member (+): ^T -> ^T -> ^T) and ^T : (static member (*):  ^T -> ^T -> ^T) and ^T : comparison> = {
        objectiveFunction: ^T[]
        contraints: (LinearPolynomial< ^T> * ProblemSense) array
        coordinateContrainsts: ((ProblemSense * ^T) option) array
    }


//    type LinearProgram =

    
            // static member op_Addition (poly1,poly2) : LinearPolynomial =
            //     match poly1 with
            //         | Constant t -> match poly2 with
            //                             | Constant u -> Constant(t + u)
            //                             | Sum(a,Constant u) -> Sum(a,Constant(t+u))
            //                             | Sum(Constant u,a) -> Sum(Constant(t+u),a)
//
    // = 0.5x_5 + 1.2x_2 + 4.2
    let lp = Sum((Sum((Variable (0.5,4)),(Variable (1.2,1)))),(Constant(4.2)))
    lp.Evaluate [|1.0;2.0;4.4;12.2|]
    let rand = new System.Random()
    for i in [|1..1000009000|] do (lp.Evaluate [|rand.NextDouble();rand.NextDouble();rand.NextDouble();rand.NextDouble()|]|> ignore)
   // let matrixPlusVector = LinearPolynomial<float>.ToMatrixPlusVector [|lp|]
 
 
 (*



## Copyright 2015-2017 Tom Brown (FIAS), Jonas Hoersch (FIAS)

## This program is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation; either version 3 of the
## License, or (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""
Tools for fast Pyomo linear problem building.

Essentially this library replaces Pyomo expressions with more strict
objects with a pre-defined affine structure.

This code is also available as a gist

https://gist.github.com/nworbmot/db3d446fa3b5c388519390e46fd5d8c3

under a more permissive Apache 2.0 licence to allow sharing with other
projects.

"""

# make the code as Python 3 compatible as possible
from __future__ import division, absolute_import
from six.moves import range

import logging
logger = logging.getLogger(__name__)


from pyomo.environ import Constraint, Objective, Var, ComponentUID
from weakref import ref as weakref_ref

import pyomo
from contextlib import contextmanager
from six import iteritems
from six.moves import cPickle as pickle
import pandas as pd
import gc, os, tempfile

__author__ = "Tom Brown (FIAS), Jonas Hoersch (FIAS)"
__copyright__ = "Copyright 2015-2017 Tom Brown (FIAS), Jonas Hoersch (FIAS), GNU GPL 3"


class LExpression(object):
    """Affine expression of optimisation variables.

    Affine expression of the form:

    constant + coeff1*var1 + coeff2*var2 + ....

    Parameters
    ----------
    variables : list of tuples of coefficients and variables
        e.g. [(coeff1,var1),(coeff2,var2),...]
    constant : float

    """

    def __init__(self,variables=None,constant=0.):

        if variables is None:
            self.variables = []
        else:
            self.variables = variables

        self.constant = constant

    def __repr__(self):
        return "{} + {}".format(self.variables, self.constant)


    def __mul__(self,constant):
        try:
            constant = float(constant)
        except:
            logger.error("Can only multiply an LExpression with a float!")
            return None
        return LExpression([(constant*item[0],item[1]) for item in self.variables],
                           constant*self.constant)

    def __rmul__(self,constant):
        return self.__mul__(constant)

    def __add__(self,other):
        if type(other) is LExpression:
            return LExpression(self.variables + other.variables,self.constant+other.constant)
        else:
            try:
                constant = float(other)
            except:
                logger.error("Can only add an LExpression to another LExpression or a constant!")
                return None
            return LExpression(self.variables[:],self.constant+constant)


    def __radd__(self,other):
        return self.__add__(other)

    def __pos__(self):
        return self

    def __neg__(self):
        return -1*self

class LConstraint(object):
    """Constraint of optimisation variables.

    Linear constraint of the form:

    lhs sense rhs

    Parameters
    ----------
    lhs : LExpression
    sense : string
    rhs : LExpression

    """

    def __init__(self,lhs=None,sense="==",rhs=None):

        if lhs is None:
            self.lhs = LExpression()
        else:
            self.lhs = lhs

        self.sense = sense

        if rhs is None:
            self.rhs = LExpression()
        else:
            self.rhs = rhs

    def __repr__(self):
        return "{} {} {}".format(self.lhs, self.sense, self.rhs)


def l_constraint(model,name,constraints,*args):
    """A replacement for pyomo's Constraint that quickly builds linear
    constraints.

    Instead of

    model.name = Constraint(index1,index2,...,rule=f)

    call instead

    l_constraint(model,name,constraints,index1,index2,...)

    where constraints is a dictionary of constraints of the form:

    constraints[i] = LConstraint object

    OR using the soon-to-be-deprecated list format:

    constraints[i] = [[(coeff1,var1),(coeff2,var2),...],sense,constant_term]

    i.e. the first argument is a list of tuples with the variables and their
    coefficients, the second argument is the sense string (must be one of
    "==","<=",">=","><") and the third argument is the constant term
    (a float). The sense "><" allows lower and upper bounds and requires
    `constant_term` to be a 2-tuple.

    Variables may be repeated with different coefficients, which pyomo
    will sum up.

    Parameters
    ----------
    model : pyomo.environ.ConcreteModel
    name : string
        Name of constraints to be constructed
    constraints : dict
        A dictionary of constraints (see format above)
    *args :
        Indices of the constraints

    """

    setattr(model,name,Constraint(*args,noruleinit=True))
    v = getattr(model,name)
    for i in v._index:
        c = constraints[i]
        if type(c) is LConstraint:
            variables = c.lhs.variables + [(-item[0],item[1]) for item in c.rhs.variables]
            sense = c.sense
            constant = c.rhs.constant - c.lhs.constant
        else:
            variables = c[0]
            sense = c[1]
            constant = c[2]

        v._data[i] = pyomo.core.base.constraint._GeneralConstraintData(None,v)
        v._data[i]._body = pyomo.core.base.expr_coopr3._SumExpression()
        v._data[i]._body._args = [item[1] for item in variables]
        v._data[i]._body._coef = [item[0] for item in variables]
        v._data[i]._body._const = 0.
        if sense == "==":
            v._data[i]._equality = True
            v._data[i]._lower = pyomo.core.base.numvalue.NumericConstant(constant)
            v._data[i]._upper = pyomo.core.base.numvalue.NumericConstant(constant)
        elif sense == "<=":
            v._data[i]._equality = False
            v._data[i]._lower = None
            v._data[i]._upper = pyomo.core.base.numvalue.NumericConstant(constant)
        elif sense == ">=":
            v._data[i]._equality = False
            v._data[i]._lower = pyomo.core.base.numvalue.NumericConstant(constant)
            v._data[i]._upper = None
        elif sense == "><":
            v._data[i]._equality = False
            v._data[i]._lower = pyomo.core.base.numvalue.NumericConstant(constant[0])
            v._data[i]._upper = pyomo.core.base.numvalue.NumericConstant(constant[1])
        else: raise KeyError('`sense` must be one of "==","<=",">=","><"; got: {}'.format(sense))

def l_objective(model,objective=None):
    """
    A replacement for pyomo's Objective that quickly builds linear
    objectives.

    Instead of

    model.objective = Objective(expr=sum(vars[i]*coeffs[i] for i in index)+constant)

    call instead

    l_objective(model,objective)

    where objective is an LExpression.

    Variables may be repeated with different coefficients, which pyomo
    will sum up.


    Parameters
    ----------
    model : pyomo.environ.ConcreteModel
    objective : LExpression

    """

    if objective is None:
        objective = LExpression()

    #initialise with a dummy
    model.objective = Objective(expr = 0.)

    model.objective._expr = pyomo.core.base.expr_coopr3._SumExpression()
    model.objective._expr._args = [item[1] for item in objective.variables]
    model.objective._expr._coef = [item[0] for item in objective.variables]
    model.objective._expr._const = objective.constant

def free_pyomo_initializers(obj):
    obj.construct()
    if isinstance(obj, Var):
        attrs = ('_bounds_init_rule', '_bounds_init_value',
                 '_domain_init_rule', '_domain_init_value',
                 '_value_init_rule', '_value_init_value')
    elif isinstance(obj, Constraint):
        attrs = ('rule', '_init_expr')
    else:
        raise NotImplemented

    for attr in attrs:
        if hasattr(obj, attr):
            setattr(obj, attr, None)

@contextmanager
def empty_model(model):
    logger.debug("Storing pyomo model to disk")
    rules = {}
    for obj in model.component_objects(ctype=Constraint):
        if obj.rule is not None:
            rules[obj.name] = obj.rule
            obj.rule = None

    bounds = {}
    for obj in model.component_objects(ctype=Var):
        if obj._bounds_init_rule is not None:
            bounds[obj.name] = obj._bounds_init_rule
            obj._bounds_init_rule = None

    smap_id, symbol_map = (next(iteritems(model.solutions.symbol_map))
               if model.solutions.symbol_map
               else (None, None))
    if smap_id is not None:
        for m in ('bySymbol', 'aliases'):
            setattr(symbol_map, m,
                    {n: ComponentUID(obj())
                     for n, obj in iteritems(getattr(symbol_map, m))})

    fd, fn = tempfile.mkstemp()
    with os.fdopen(fd, 'wb') as f:
        pickle.dump(model.__getstate__(), f, -1)

    model.__dict__.clear()
    logger.debug("Stored pyomo model to disk")

    gc.collect()
    yield

    logger.debug("Reloading pyomo model")
    with open(fn, 'rb') as f:
        state = pickle.load(f)
    os.remove(fn)
    model.__setstate__(state)

    for n, rule in iteritems(rules):
        getattr(model, n).rule = rule

    for n, bound in iteritems(bounds):
        getattr(model, n)._bounds_init_rule = bound

    if smap_id is not None:
        for m in ('bySymbol', 'aliases'):
            setattr(symbol_map, m,
                    {n: weakref_ref(cuid.find_component(model))
                     for n, cuid in iteritems(getattr(symbol_map, m))})
        symbol_map.byObject = {id(obj()): symb
                               for symb, obj in iteritems(symbol_map.bySymbol)}
        model.solutions.symbol_map[smap_id] = symbol_map
    logger.debug("Reloaded pyomo model")

@contextmanager
def empty_network(network):
    logger.debug("Storing pypsa timeseries to disk")

    panels = {}
    for c in network.all_components:
        attr = network.components[c]["list_name"] + "_t"
        panels[attr] = getattr(network, attr)
        setattr(network, attr, None)

    fd, fn = tempfile.mkstemp()
    with os.fdopen(fd, 'wb') as f:
        pickle.dump(panels, f, -1)

    del panels

    gc.collect()
    yield

    logger.debug("Reloading pypsa timeseries from disk")
    with open(fn, 'rb') as f:
        panels = pickle.load(f)
    os.remove(fn)
    for attr, pnl in iteritems(panels):
        setattr(network, attr, pnl)

def patch_optsolver_free_model_before_solving(opt, model):
    orig_apply_solver = opt._apply_solver
    def wrapper():
        with empty_model(model):
            return orig_apply_solver()
    opt._apply_solver = wrapper

def patch_optsolver_record_memusage_before_solving(opt, network):
    try:
        import resource

        orig_apply_solver = opt._apply_solver
        def wrapper():
            network.max_memusage = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
            return orig_apply_solver()
        opt._apply_solver = wrapper
        return True
    except ImportError:
        logger.debug("Unable to measure memory usage, since the resource library is missing")
        return False


*)