/**
 * Inflect — 40 ejercicios resueltos de validación de campos (TypeScript).
 *
 * Convenciones:
 * - RegExp ASCII para letras/dígitos salvo que el enunciado indique Unicode.
 * - Longitud = string.length (unidades UTF-16), excepto #39 (code points).
 * - Las funciones devuelven boolean y no modifican su argumento.
 * - Revisa un ejercicio, predice su resultado y ejecuta las pruebas al final.
 */

// 01. Contiene al menos una mayúscula ASCII.
export const e01Mayuscula = (s: string): boolean => /[A-Z]/.test(s);

// 02. Contiene al menos una minúscula ASCII.
export const e02Minuscula = (s: string): boolean => /[a-z]/.test(s);

// 03. Contiene al menos un dígito.
export const e03Digito = (s: string): boolean => /[0-9]/.test(s);

// 04. Contiene al menos un carácter especial (no letra ASCII ni dígito).
export const e04Especial = (s: string): boolean => /[^A-Za-z0-9]/.test(s);

// 05. Longitud estrictamente mayor que 9 y menor que 32 (10..31).
export const e05Longitud = (s: string): boolean => s.length > 9 && s.length < 32;

// 06. No empieza ni termina con guion bajo; exige string no vacío.
export const e06SinGuionBajoExtremos = (s: string): boolean =>
  s.length > 0 && !s.startsWith("_") && !s.endsWith("_");

// 07. Campo obligatorio sin solo espacios.
export const e07Requerido = (s: string): boolean => s.trim().length > 0;

// 08. Solo letras ASCII (al menos una).
export const e08SoloLetras = (s: string): boolean => /^[A-Za-z]+$/.test(s);

// 09. Solo números ASCII (al menos uno).
export const e09SoloNumeros = (s: string): boolean => /^[0-9]+$/.test(s);

// 10. Solo letras, dígitos y guiones bajos (al menos uno).
export const e10AlfanumericoGuion = (s: string): boolean => /^\w+$/.test(s);

// 11. No tiene espacios en blanco (admite string vacío).
export const e11SinEspacios = (s: string): boolean => !/\s/.test(s);

// 12. No tiene dos guiones bajos consecutivos.
export const e12SinDobleGuionBajo = (s: string): boolean => !s.includes("__");

// 13. Empieza por letra ASCII.
export const e13EmpiezaConLetra = (s: string): boolean => /^[A-Za-z]/.test(s);

// 14. Termina en dígito.
export const e14TerminaConDigito = (s: string): boolean => /[0-9]$/.test(s);

// 15. Exactamente 6 dígitos (código de verificación).
export const e15CodigoSeisDigitos = (s: string): boolean => /^[0-9]{6}$/.test(s);

// 16. Código postal mexicano de 5 dígitos (validación de formato).
export const e16CodigoPostalMx = (s: string): boolean => /^[0-9]{5}$/.test(s);

// 17. Nombre de usuario: 3..20 caracteres ASCII, inicia con letra;
//     solo letras, dígitos y _; no termina en _ ni contiene __.
export const e17Usuario = (s: string): boolean =>
  /^[A-Za-z][A-Za-z0-9_]{2,19}$/.test(s) &&
  !s.endsWith("_") && !s.includes("__");

// 18. Contraseña compuesta: 10..31, mayúscula, minúscula, dígito,
//     especial y ningún espacio; no comienza ni termina con _.
export const e18Password = (s: string): boolean =>
  e05Longitud(s) && e01Mayuscula(s) && e02Minuscula(s) &&
  e03Digito(s) && e04Especial(s) && e11SinEspacios(s) &&
  e06SinGuionBajoExtremos(s);

// 19. Correo: comprobación básica de formato, NO confirma existencia.
export const e19EmailBasico = (s: string): boolean =>
  /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(s);

// 20. URL web HTTP(S) con hostname según URL nativa.
export const e20UrlHttp = (s: string): boolean => {
  try {
    const url = new URL(s);
    return (url.protocol === "http:" || url.protocol === "https:") &&
      url.hostname.includes(".");
  } catch {
    return false;
  }
};

// 21. Fecha ISO YYYY-MM-DD realmente existente.
export const e21FechaIso = (s: string): boolean => {
  if (!/^\d{4}-\d{2}-\d{2}$/.test(s)) return false;
  const [year, month, day] = s.split("-").map(Number);
  if (year < 1 || month < 1 || month > 12 || day < 1) return false;
  const days = new Date(Date.UTC(year, month, 0)).getUTCDate();
  return day <= days;
};

// 22. Hora 24h HH:MM de 00:00 a 23:59.
export const e22Hora24 = (s: string): boolean =>
  /^([01][0-9]|2[0-3]):[0-5][0-9]$/.test(s);

// 23. Teléfono México: exactamente 10 dígitos (sin prefijo).
export const e23TelefonoMx = (s: string): boolean => /^\d{10}$/.test(s);

// 24. RFC persona física: formato superficial, sin validar SAT/homoclave.
export const e24RfcFormato = (s: string): boolean =>
  /^[A-ZÑ&]{4}\d{6}[A-Z0-9]{3}$/.test(s);

// 25. No contiene etiquetas HTML evidentes. NO sanitiza HTML ni previene XSS.
export const e25SinTagsHtmlSimples = (s: string): boolean => !/<[^>]*>/.test(s);

// 26. Cantidad de caracteres numéricos al menos 2.
export const e26DosDigitos = (s: string): boolean =>
  (s.match(/[0-9]/g) ?? []).length >= 2;

// 27. Exactamente una @.
export const e27UnaArroba = (s: string): boolean =>
  (s.match(/@/g) ?? []).length === 1;

// 28. Sin caracteres repetidos tres veces seguidas (cualquier carácter).
export const e28SinTriples = (s: string): boolean => !/(.)\1\1/u.test(s);

// 29. Longitud entre 5 y 12 inclusiva.
export const e29LongitudInclusiva = (s: string): boolean =>
  s.length >= 5 && s.length <= 12;

// 30. Prefijo API- seguido de 8 letras mayúsculas o dígitos.
export const e30ApiKeyFormato = (s: string): boolean =>
  /^API-[A-Z0-9]{8}$/.test(s);

// 31. Monto decimal no negativo, hasta dos decimales; sin signo ni comas.
export const e31Monto = (s: string): boolean =>
  /^(0|[1-9]\d*)(\.\d{1,2})?$/.test(s);

// 32. Entero entre 18 y 120, sin ceros iniciales.
export const e32Edad = (s: string): boolean =>
  /^(0|[1-9]\d*)$/.test(s) && Number(s) >= 18 && Number(s) <= 120;

// 33. Valores permitidos explícitos (case sensitive).
export const e33Estado = (s: string): boolean =>
  ["pending", "approved", "rejected"].includes(s);

// 34. Código hex RGB #RRGGBB.
export const e34ColorHex = (s: string): boolean => /^#[0-9a-fA-F]{6}$/.test(s);

// 35. UUID versión 4 (formato y bits de variante).
export const e35UuidV4 = (s: string): boolean =>
  /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/i.test(s);

// 36. Slug web lowercase: letras, números, guiones internos; sin --.
export const e36Slug = (s: string): boolean =>
  /^[a-z0-9]+(?:-[a-z0-9]+)*$/.test(s);

// 37. Palabra palíndroma (ignora mayúsculas, espacios y puntuación ASCII).
export const e37Palindromo = (s: string): boolean => {
  const normalized = s.toLowerCase().replace(/[^a-z0-9]/g, "");
  return normalized.length > 0 &&
    normalized === [...normalized].reverse().join("");
};

// 38. Nombre con letras Unicode, espacios interiores simples y acentos;
//     evita espacios al principio/final y números.
export const e38NombreUnicode = (s: string): boolean =>
  /^\p{L}+(?: \p{L}+)*$/u.test(s);

// 39. Entre 3 y 10 code points Unicode (emoji cuenta como uno en muchos casos).
export const e39LongitudUnicode = (s: string): boolean => {
  const length = [...s].length;
  return length >= 3 && length <= 10;
};

// 40. Validar todos los campos de un formulario con errores específicos.
export type Registro = { username: string; password: string; email: string; age: string };
export const e40ValidarRegistro = (r: Registro): string[] => {
  const errors: string[] = [];
  if (!e17Usuario(r.username)) errors.push("username");
  if (!e18Password(r.password)) errors.push("password");
  if (!e19EmailBasico(r.email)) errors.push("email");
  if (!e32Edad(r.age)) errors.push("age");
  return errors;
};
