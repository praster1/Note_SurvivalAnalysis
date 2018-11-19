import numpy as np
import matplotlib as mpl
mpl.use('pgf')

def figsize(width, height=None):
    fig_width_pt = 426.79135                        # Get this from LaTeX using \the\textwidth
    inches_per_pt = 1.0/72.27                       # Convert pt to inch
    golden_mean = (np.sqrt(5.0)-1.0)/2.0            # Aesthetic ratio (you could change this)
    fig_width = fig_width_pt*inches_per_pt*width    # width in inches
    fig_height = fig_width*golden_mean              # height in inches
    if height is not None:
        fig_height = (height/width) * fig_width
    fig_size = [fig_width,fig_height]
    return fig_size

pgf_with_latex = {                      # setup matplotlib to use latex for output
    "pgf.texsystem": "pdflatex",        # change this if using xetex or lautex
    "text.usetex": True,                # use LaTeX to write all text
    "font.family": "sans-serif",
    "font.serif": [],                   # blank entries should cause plots to inherit fonts from the document
    "font.sans-serif": [],
    "font.monospace": [],
    "axes.labelsize": 10,               # LaTeX default is 10pt font.
    "font.size": 10,
    "legend.fontsize": 8,               # Make the legend/label fonts a little smaller
    "xtick.labelsize": 8,
    "ytick.labelsize": 8,
    "figure.figsize": figsize(0.9),     # default fig size of 0.9 textwidth
    "pgf.preamble": [
        r"\usepackage[utf8x]{inputenc}",
        r"\usepackage[T1]{fontenc}",
        ]
    }
mpl.rcParams.update(pgf_with_latex)

import matplotlib.pyplot as plt

def newfig(width, height=None, ax_pos=111):
    plt.clf()
    fig = plt.figure(figsize=figsize(width, height))
    ax = fig.add_subplot(ax_pos)
    return fig, ax


def savefig(filename):
    plt.savefig('../../plots/{}.pgf'.format(filename))
    plt.savefig('../../plots/{}.pdf'.format(filename))

