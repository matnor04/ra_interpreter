/**
 * ==============================================================================
 * Álgebra Relacional Studio - Frontend Client Logic
 * ==============================================================================
 */

// Estado global de la aplicación
let editor = null;
let ultimoResultado = [];

// ==============================================================================
// 1. Inicialización del Editor (CodeMirror & Fallbacks)
// ==============================================================================
function inicializarEditor() {
    const textarea = document.getElementById('codigo');

    try {
        if (typeof CodeMirror !== 'undefined') {
            editor = CodeMirror.fromTextArea(textarea, {
                mode: 'text/x-sql',
                theme: 'dracula',
                lineNumbers: true,
                autoCloseBrackets: true,
                extraKeys: {
                    "Ctrl-Enter": function() { ejecutar(); },
                    "Cmd-Enter": function() { ejecutar(); }
                }
            });
        }
    } catch (e) {
        console.warn("CodeMirror no disponible, utilizando textarea estándar:", e);
    }

    // Atajo fallback para textarea nativo si CodeMirror no estuviera disponible
    textarea.addEventListener('keydown', function(e) {
        if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
            e.preventDefault();
            ejecutar();
        }
    });
}

// ==============================================================================
// 2. Gestión de Catálogo de Tablas
// ==============================================================================
async function cargarCatalogo() {
    const container = document.getElementById('lista-tablas');

    try {
        const res = await fetch('/api/catalog');
        const tablas = await res.json();
        container.innerHTML = '';

        tablas.forEach(t => {
            container.innerHTML += `
                <div class="badge" onclick="verTabla('${t}')" title="Ver contenido de ${t}">
                    <span>🗄️ ${t}</span>
                    <span style="font-size: 11px; color: var(--text-muted);">tabla</span>
                </div>`;
        });
    } catch (e) {
        container.innerHTML = '<div class="feedback-box feedback-error">Error al conectar con el servidor</div>';
    }
}

function verTabla(nombre) {
    if (editor) {
        editor.setValue(nombre);
        editor.focus();
    } else {
        document.getElementById('codigo').value = nombre;
    }
    ejecutar();
}

function abrirSelectorCSV() {
    const input = document.getElementById('input-csv');
    input.value = '';
    input.click();
}

async function procesarArchivoCSV(event) {
    const file = event.target.files[0];
    if (!file) return;

    // Obtener nombre base sin .csv y sanitizar para identificadores de AR
    const rawName = file.name.replace(/\.[^/.]+$/, "");
    let sugerencia = rawName.trim().replace(/[^a-zA-Z0-9_]/g, "_");
    if (/^[0-9]/.test(sugerencia)) {
        sugerencia = "T_" + sugerencia;
    }

    const nombreTabla = prompt("Nombre para la relación en Álgebra Relacional:", sugerencia);
    if (!nombreTabla || nombreTabla.trim() === "") return;

    let cleanNom = nombreTabla.trim().replace(/[^a-zA-Z0-9_]/g, "_");
    if (/^[0-9]/.test(cleanNom)) {
        cleanNom = "T_" + cleanNom;
    }

    const divRes = document.getElementById('resultado');
    divRes.innerHTML = `<div style='color: var(--text-muted); font-size: 14px;'>⏳ Subiendo y procesando ${file.name}...</div>`;

    try {
        const contenido = await file.text();
        const response = await fetch('/api/upload', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ nombre: cleanNom, contenido: contenido })
        });
        const res = await response.json();

        if (res.status === 'ok') {
            await cargarCatalogo();
            divRes.innerHTML = `
                <div class="feedback-box feedback-success">
                    <span>✅</span>
                    <div>${res.msg}</div>
                </div>`;
            verTabla(res.nombre);
        } else {
            divRes.innerHTML = `
                <div class="feedback-box feedback-error">
                    <span>❌</span>
                    <div><strong>Error al subir archivo:</strong><br>${res.msg}</div>
                </div>`;
        }
    } catch (e) {
        divRes.innerHTML = `
            <div class="feedback-box feedback-error">
                <span>❌</span>
                <div><strong>Error:</strong> Falló la comunicación con el servidor al subir el archivo.</div>
            </div>`;
    }
}

// ==============================================================================
// 3. Ejecución de Consultas AR
// ==============================================================================
async function ejecutar() {
    const rawCodigo = editor ? editor.getValue() : document.getElementById('codigo').value;
    const codigo = rawCodigo.trim();
    if (codigo === "") return;

    const divRes = document.getElementById('resultado');
    divRes.innerHTML = "<div style='color: var(--text-muted); font-size: 14px;'>⏳ Evaluando expresión en servidor Haskell...</div>";

    try {
        const url = '/api/ejecutar?consulta=' + encodeURIComponent(codigo);
        const response = await fetch(url, { method: 'POST' });
        const res = await response.json();

        if (res.status === 'error') {
            divRes.innerHTML = `
                <div class="feedback-box feedback-error">
                    <span>❌</span>
                    <div><strong>Error de evaluación:</strong><br><span style="font-family: var(--font-code); font-size: 12.5px;">${res.msg}</span></div>
                </div>`;
        } else if (res.status === 'info') {
            await cargarCatalogo(); // Recargar el catálogo si hubo asignaciones
            divRes.innerHTML = `
                <div class="feedback-box feedback-success">
                    <span>✅</span>
                    <div>${res.msg}</div>
                </div>`;
        } else {
            dibujarTabla(res.data, divRes);
        }
    } catch (e) {
        divRes.innerHTML = `
            <div class="feedback-box feedback-error">
                <span>❌</span>
                <div><strong>Error crítico:</strong> No se pudo comunicar con el servidor backend.</div>
            </div>`;
    }
}

// ==============================================================================
// 4. Renderizado de Tablas de Resultados
// ==============================================================================
function dibujarTabla(filas, contenedor) {
    ultimoResultado = filas;

    if (!filas || filas.length === 0) {
        contenedor.innerHTML = `
            <div class="feedback-box feedback-success">
                <span>✅</span>
                <div>Consulta exitosa (0 tuplas resultantes).</div>
            </div>`;
        return;
    }

    const columnas = Object.keys(filas[0]);

    let html = `
        <div class="results-header">
            <div class="results-actions">
                <button class="button-export csv" onclick="descargarCSV()" title="Descargar como CSV (RFC 4180 / UTF-8)">
                    <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                        <path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"></path>
                        <polyline points="14 2 14 8 20 8"></polyline>
                        <line x1="8" y1="13" x2="16" y2="13"></line>
                        <line x1="8" y1="17" x2="16" y2="17"></line>
                    </svg>
                    <span>Descargar CSV</span>
                </button>
                <button class="button-export excel" onclick="descargarExcel()" title="Descargar como libro de Excel (.xls)">
                    <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
                        <rect x="3" y="3" width="18" height="18" rx="2" ry="2"></rect>
                        <line x1="3" y1="9" x2="21" y2="9"></line>
                        <line x1="3" y1="15" x2="21" y2="15"></line>
                        <line x1="9" y1="3" x2="9" y2="21"></line>
                        <line x1="15" y1="3" x2="15" y2="21"></line>
                    </svg>
                    <span>Descargar Excel</span>
                </button>
            </div>
        </div>
        <div class='table-container'>
            <table>
                <thead>
                    <tr>`;

    columnas.forEach(col => html += `<th>${col}</th>`);
    html += `       </tr>
                </thead>
                <tbody>`;

    filas.forEach(fila => {
        html += "<tr>";
        columnas.forEach(col => {
            const val = fila[col];
            html += `<td>${val !== undefined && val !== null ? val : ''}</td>`;
        });
        html += "</tr>";
    });

    html += "</tbody></table></div>";
    contenedor.innerHTML = html;
}

// ==============================================================================
// 5. Exportación de Resultados (CSV y Excel)
// ==============================================================================
function descargarCSV() {
    if (!ultimoResultado || ultimoResultado.length === 0) return;

    const columnas = Object.keys(ultimoResultado[0]);
    const escaparCSV = (val) => {
        if (val === null || val === undefined) return '';
        const str = String(val);
        if (str.includes(',') || str.includes('"') || str.includes('\n') || str.includes('\r')) {
            return '"' + str.replace(/"/g, '""') + '"';
        }
        return str;
    };

    const lineas = [
        columnas.map(escaparCSV).join(','),
        ...ultimoResultado.map(fila => columnas.map(col => escaparCSV(fila[col])).join(','))
    ];

    // Se agrega UTF-8 BOM (\uFEFF) para compatibilidad total con Excel y Numbers
    const blob = new Blob(["\uFEFF" + lineas.join('\r\n')], { type: 'text/csv;charset=utf-8;' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `resultado_consulta_${new Date().toISOString().slice(0, 10)}.csv`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
}

function descargarExcel() {
    if (!ultimoResultado || ultimoResultado.length === 0) return;

    const columnas = Object.keys(ultimoResultado[0]);

    let html = '<html xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:x="urn:schemas-microsoft-com:office:excel" xmlns="http://www.w3.org/TR/REC-html40">';
    html += '<head><meta charset="utf-8"><!--[if gte mso 9]><xml><x:ExcelWorkbook><x:ExcelWorksheets><x:ExcelWorksheet><x:Name>Resultados</x:Name><x:WorksheetOptions><x:DisplayGridlines/></x:WorksheetOptions></x:ExcelWorksheet></x:ExcelWorksheets></x:ExcelWorkbook></xml><![endif]--></head><body><table border="1"><thead><tr>';
    columnas.forEach(col => html += '<th style="background-color: #2d3748; color: #ffffff; font-weight: bold;">' + col + '</th>');
    html += '</tr></thead><tbody>';

    ultimoResultado.forEach(fila => {
        html += '<tr>';
        columnas.forEach(col => {
            const val = fila[col] !== undefined && fila[col] !== null ? fila[col] : '';
            html += '<td>' + val + '</td>';
        });
        html += '</tr>';
    });

    html += '</tbody></table></body></html>';

    const blob = new Blob([html], { type: 'application/vnd.ms-excel;charset=utf-8;' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `resultado_consulta_${new Date().toISOString().slice(0, 10)}.xls`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
}

// ==============================================================================
// 6. Ciclo de Vida inicial
// ==============================================================================
window.onload = function() {
    inicializarEditor();
    cargarCatalogo();
};
