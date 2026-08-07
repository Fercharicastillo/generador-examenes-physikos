\documentclass[11pt,a4paper]{article}

\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[spanish]{babel}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{textcomp}
\usepackage{xcolor}
\usepackage[
  top=1.6cm,
  bottom=1.6cm,
  left=2cm,
  right=2cm
]{geometry}

\definecolor{physikosblue}{RGB}{16, 101, 170}

\setlength{\parindent}{0pt}
\setlength{\parskip}{4pt}

\begin{document}

\begin{center}
  {\color{physikosblue}
    \Large\bfseries @@TIPO_DOCUMENTO@@
  }

  {\small
    Pregunta: \texttt{@@IDENTIFICADOR@@}
  }

  \vspace{4pt}

  {\color{physikosblue}\hrule}
\end{center}

\vspace{8pt}

\small

@@CONTENIDO@@

\end{document}