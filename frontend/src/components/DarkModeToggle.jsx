// CODEX: añadido para mostrar el control de modo oscuro de Physikos.
export function DarkModeToggle({ isDarkMode, onChange }) {
  const label = isDarkMode
    ? "Cambiar a modo claro"
    : "Cambiar a modo oscuro";

  return (
    <div className="content__DarkMode">
      <label
        className={`darmode-btn-content ${
          isDarkMode ? "btn-cambio-after-dM" : ""
        }`}
        title={label}
      >
        <input
          id="click-DarkMode"
          type="checkbox"
          checked={isDarkMode}
          onChange={onChange}
          aria-label={label}
        />
        <span className="btn-sun" aria-hidden="true" />
        <span className="darkmode-track" aria-hidden="true">
          <span className="darkmode-thumb" />
        </span>
        <span className="btn-moon" aria-hidden="true" />
      </label>
    </div>
  );
}
