import { useEffect, useMemo, useState } from "react";
import {
  construirUrlDescarga,
  generarExamenes,
  obtenerPlantillas,
  verificarSalud,
} from "./api";
import { DarkModeToggle } from "./components/DarkModeToggle";
import { useDarkMode } from "./hooks/useDarkMode";
import "./App.css";

const PHYSIKOS_URL =
  import.meta.env.VITE_PHYSIKOS_URL ||
  "https://fercharicastillo.github.io/chari/";

function App() {
  const { isDarkMode, toggleDarkMode } = useDarkMode();
  const [plantillas, setPlantillas] = useState([]);
  const [plantilla, setPlantilla] = useState("1");

  const [textoEstudiantes, setTextoEstudiantes] = useState(
    [
      "Almachi Moreno Consuelo Maribel",
      "Baez Rios Karen Daniela",
    ].join("\n")
  );

  const [incluirSoluciones, setIncluirSoluciones] =
    useState(true);

  const [semilla, setSemilla] = useState("20260804");
  const [cargando, setCargando] = useState(false);
  const [error, setError] = useState("");
  const [resultado, setResultado] = useState(null);
  const [menuAbierto, setMenuAbierto] = useState(false);

  const [estadoMotor, setEstadoMotor] =
  useState("conectando");

  const estudiantes = useMemo(() => {
    return textoEstudiantes
      .split("\n")
      .map((nombre) => nombre.trim())
      .filter(Boolean);
  }, [textoEstudiantes]);

  useEffect(() => {
  // CODEX: añadido para comprobar la API antes de cargar plantillas.
  async function iniciarAplicacion() {
    setEstadoMotor("conectando");
    setError("");

    try {
      const salud = await verificarSalud();

      const estadoSalud = Array.isArray(salud.estado)
        ? salud.estado[0]
        : salud.estado;

      if (estadoSalud !== "ok") {
        throw new Error(
          "El motor respondió con un estado inesperado."
        );
      }

      setEstadoMotor("conectado");

      const respuesta = await obtenerPlantillas();
      const disponibles = respuesta.plantillas || [];

      setPlantillas(disponibles);

      if (disponibles.length > 0) {
        const primerId = Array.isArray(disponibles[0].id)
          ? disponibles[0].id[0]
          : disponibles[0].id;

        setPlantilla(String(primerId));
      }
    } catch (error) {
      setEstadoMotor("desconectado");
      setPlantillas([]);

      setError(
        `No fue posible conectar con el motor: ${error.message}`
      );
    }
  }

  iniciarAplicacion();
}, []);


  async function manejarGeneracion(evento) {
    evento.preventDefault();

    setError("");
    setResultado(null);

    if (estudiantes.length === 0) {
      setError("Escribe al menos un estudiante.");
      return;
    }

    const semillaNumerica = Number(semilla);

    if (!Number.isInteger(semillaNumerica)) {
      setError("La semilla debe ser un número entero.");
      return;
    }

    setCargando(true);

    try {
      const respuesta = await generarExamenes({
        plantilla: Number(plantilla),
        estudiantes,
        incluir_soluciones: incluirSoluciones,
        semilla: semillaNumerica,
      });

      setResultado(respuesta);
    } catch (error) {
      setError(error.message);
    } finally {
      setCargando(false);
    }
  }

const informacionMotor = {
  conectando: {
    texto: "Conectando...",
    clase: "status-connecting",
  },
  conectado: {
    texto: "Motor conectado",
    clase: "status-connected",
  },
  desconectado: {
    texto: "Motor no disponible",
    clase: "status-disconnected",
  },
}[estadoMotor];

  return (
    <div className="app-shell">
      <div className="app-header">
        <div className="app-header__content-namepage">
          <a
            className="app-header__brand"
            href={PHYSIKOS_URL}
            title="Ir a Physikos"
          >
            <div
              className="app-header__logo"
              alt="Physikos"
            ></div>
          </a>
          <div
            id="btn_menu"
            className="app-header__btnmenu"
            type="button"
            aria-label={menuAbierto ? "Cerrar menú" : "Abrir menú"}
            aria-controls="menu-principal"
            aria-expanded={menuAbierto}
            onClick={() => setMenuAbierto((abierto) => !abierto)}
          />
        </div>
        <div className="app-header__content-actions">
          <DarkModeToggle
            isDarkMode={isDarkMode}
            onChange={toggleDarkMode}
          />
        </div>
      </div>

      <aside
        id="menu-principal"
        className={`menu ${menuAbierto ? "menu--open" : ""}`}
      >
        <nav className="menu-nav">
          <div className="menu-nav__content">
            <p className="menu-nav__content-inicio"><a href="#">Inicio</a></p>
            <p className="menu-nav__content-preguntas"><a href="#">Preguntas</a></p>
            <p className="current current-part menu-nav__content-generador"><a href="#">Generador</a></p>
            <p className="menu-nav__content-plantillas"><a href="#">Plantillas</a></p>
          </div>
        </nav>
      </aside>

      <div
        className={`menu-overlay ${menuAbierto ? "menu-overlay--visible" : ""}`}
        type="button"
        aria-label="Cerrar menú"
        tabIndex={menuAbierto ? 0 : -1}
        onClick={() => setMenuAbierto(false)}
      />

      <div
        className={`main-content ${menuAbierto ? "main_ds" : ""}`}
        aria-label={menuAbierto ? "Exapandir main" : "Contraer main"}
      >
        <header className="generador-profile-section generador-page__hero">
          <span className="generador-profile-section__icon" aria-hidden="true"></span>
          <div className="generador-page__hero-content">
              <h1 id="generador-page-title" className="generador-profile-section__title">Generador de exámenes aleatorios</h1>
              <p className="generador-profile-section__note">Crea evaluaciones individualizadas y sus solucionarios.</p>
              <p className="generador-profile-section__introduction">Physikós es una herramienta en constante desarrollo concebida como un recurso integral para docentes de Física y Matemática. Diseñada para optimizar el trabajo pedagógico, actualmente permite crear evaluaciones adaptadas y, próximamente, estará disponible como una aplicación de escritorio independiente.</p>
          </div>
          <div className="generador-page__hero-illustration" aria-hidden="true"></div>
        </header>

        <div className="layout-grid">
          <form
            className="panel"
            onSubmit={manejarGeneracion}
          >
            <div className="panel-heading">
              <div>
                <span className="step">1</span>
                <h2>Configurar evaluación</h2>
              </div>

              <span className={`status ${informacionMotor.clase}`} role="status" aria-live="polite">
              <span className="status-dot" aria-hidden="true"/>{informacionMotor.texto}</span>
            </div>

            <label className="field">
              <span>Plantilla</span>

              <select
                value={plantilla}
                onChange={(event) =>
                  setPlantilla(event.target.value)
                }
                disabled={cargando}
              >
                {plantillas.length === 0 && (
                  <option value="1">Plantilla 1</option>
                )}

                {plantillas.map((item) => (
                  <option key={item.id} value={item.id}>
                    {item.nombre}
                    {item.tiene_solucion
                      ? " · con solución"
                      : ""}
                  </option>
                ))}
              </select>
            </label>

            <label className="field">
              <span>Estudiantes</span>

              <textarea
                rows="10"
                value={textoEstudiantes}
                onChange={(event) =>
                  setTextoEstudiantes(event.target.value)
                }
                disabled={cargando}
                placeholder="Escribe un estudiante por línea"
              />

              <small>
                {estudiantes.length} estudiante
                {estudiantes.length === 1 ? "" : "s"}
              </small>
            </label>

            <div className="two-columns">
              <label className="field">
                <span>Semilla inicial</span>

                <input
                  type="number"
                  value={semilla}
                  onChange={(event) =>
                    setSemilla(event.target.value)
                  }
                  disabled={cargando}
                />
              </label>

              <label className="checkbox-field">
                <input
                  type="checkbox"
                  checked={incluirSoluciones}
                  onChange={(event) =>
                    setIncluirSoluciones(
                      event.target.checked
                    )
                  }
                  disabled={cargando}
                />

                <span>Incluir soluciones</span>
              </label>
            </div>

            {error && (
              <div className="message message-error">
                <strong>No se pudo generar:</strong>
                <span>{error}</span>
              </div>
            )}

            <button
              className="primary-button"
              type="submit"
              disabled={
                cargando || estadoMotor !== "conectado"
              }
            >
              {estadoMotor === "conectando"
                ? "Conectando con el motor..."
                : estadoMotor === "desconectado"
                  ? "Motor no disponible"
                  : cargando
                    ? "Generando PDF..."
                    : `           Generar ${estudiantes.length} ${
                        estudiantes.length === 1
                          ? "evaluación"
                          : "evaluaciones"
                      }`}
            </button>
          </form>

          <section className="panel result-panel">
            <div className="panel-heading">
              <div>
                <span className="step">2</span>
                <h2>Resultado</h2>
              </div>
            </div>

            {!resultado && !cargando && (
              <div className="empty-state">
                <div className="document-icon">PDF</div>

                <h3>Tu evaluación aparecerá aquí</h3>

                <p>
                  Configura los estudiantes y pulsa el botón
                  Generar.
                </p>
              </div>
            )}

            {cargando && (
              <div className="empty-state">
                <div className="loader" />

                <h3>Generando documentos</h3>

                <p>
                  R está procesando las plantillas y compilando
                  los PDF.
                </p>
              </div>
            )}

            {resultado && (
              <div className="success-card">
                <div className="success-icon">✓</div>

                <p className="eyebrow">PROCESO COMPLETADO</p>

                <h3>Evaluaciones generadas</h3>

                <dl className="result-details">
                  <div>
                    <dt>Trabajo</dt>
                    <dd>{resultado.trabajo_id}</dd>
                  </div>

                  <div>
                    <dt>Estudiantes</dt>
                    <dd>{resultado.estudiantes}</dd>
                  </div>

                  <div>
                    <dt>Exámenes</dt>
                    <dd>{resultado.examenes_generados}</dd>
                  </div>

                  <div>
                    <dt>Soluciones</dt>
                    <dd>
                      {resultado.soluciones_incluidas
                        ? "Incluidas"
                        : "No incluidas"}
                    </dd>
                  </div>
                </dl>

                <a
                  className="download-button"
                  href={construirUrlDescarga(
                    resultado.descarga
                  )}
                >
                  Descargar archivo ZIP
                </a>

                <small>{resultado.nombre_archivo}</small>
              </div>
            )}
          </section>
        </div>
      </div>

      <footer className="footer">
        <div className="footer-left">
          <a href="#">Licencia</a>
          <a href="#">| Git Hub</a>
          <a href="#">| Versiones</a>
        </div>
        <div className="footer-right"></div>
      </footer>

    </div>
  );
}

export default App;
