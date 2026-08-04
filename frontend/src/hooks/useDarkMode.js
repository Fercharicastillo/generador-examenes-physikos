import { useEffect, useState } from "react";

// CODEX: adaptado para controlar el modo oscuro desde el estado de React.
export function useDarkMode() {
  const [isDarkMode, setIsDarkMode] = useState(() => {
    return sessionStorage.getItem("darkMode") === "true";
  });

  useEffect(() => {
    if (isDarkMode) {
      document.documentElement.setAttribute("cambio", "darkMode");
    } else {
      document.documentElement.removeAttribute("cambio");
    }

    sessionStorage.setItem("darkMode", String(isDarkMode));
  }, [isDarkMode]);

  function toggleDarkMode() {
    setIsDarkMode((currentValue) => !currentValue);
  }

  return { isDarkMode, toggleDarkMode };
}
