\documentclass[12pt,a4paper]{article}

\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[spanish]{babel}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{textcomp}
\usepackage{xcolor}
\usepackage[
  top=2cm,
  bottom=2cm,
  left=2.2cm,
  right=2.2cm
]{geometry}

\definecolor{physikosblue}{RGB}{16, 101, 170}

\setlength{\parindent}{0pt}
\setlength{\parskip}{6pt}

\begin{document}

\begin{center}
  {\color{physikosblue}
    \LARGE\bfseries @@TIPO_DOCUMENTO@@
  }

  \vspace{3pt}

  {\small
    Identificador: \texttt{@@IDENTIFICADOR@@}
  }

  \vspace{6pt}

  {\color{physikosblue}\hrule}
\end{center}

\vspace{10pt}

@@CONTENIDO@@

\end{document}