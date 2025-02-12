import requests
from bs4 import BeautifulSoup
import csv
import pandas as pd

# Lista de URLs para processar
urls = [
    "https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/questionnaires.aspx?Cycle=2017-2020",
    "https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/labmethods.aspx?Cycle=2017-2020",
    "https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/manuals.aspx?Cycle=2017-2020"
]

for url in urls:
    ciclo = url.split("Cycle=")[-1]
    categoria = url.split("continuousnhanes/")[-1].split(".aspx")[0]
    
    print(f"\nProcessando categoria: {categoria} - Ciclo: {ciclo}")
    
    response = requests.get(url)
    response.encoding = 'utf-8'
    soup = BeautifulSoup(response.text, 'html.parser')

    nome_arquivo = f'{categoria}_{ciclo}.csv'
    dados = []

    # Para 'labmethods', extração baseada na tabela
    if categoria.lower() == "labmethods":
        table = soup.find('table', class_="doc-table")
        if not table:
            print("Não foi possível encontrar a tabela com os dados na página:", url)
            continue

        tbody = table.find("tbody")
        rows = tbody.find_all("tr") if tbody else table.find_all("tr")

        for row in rows:
            year_td = row.find("td", class_="table-years")
            name_td = row.find("td", class_="table-name")
            file_td = row.find("td", class_="table-file")
            date_td = row.find("td", class_="table-date")

            year = year_td.get_text(strip=True) if year_td else ""
            lab_method_name = name_td.get_text(strip=True) if name_td else ""
            release_date = date_td.get_text(strip=True) if date_td else ""

            pdf_link = ""
            documentation = ""

            if file_td:
                a_tag = file_td.find("a")
                if a_tag and a_tag.has_attr("href"):
                    pdf_link = a_tag["href"]
                    if pdf_link.startswith('/'):
                        pdf_link = "https://wwwn.cdc.gov" + pdf_link
                    documentation = a_tag.get_text(strip=True)
                else:
                    documentation = file_td.get_text(strip=True)

            dados.append({
                'Years': year,
                'Lab Method Name': lab_method_name,
                'Documentation': documentation,
                'Release Date': release_date,
                'PDF Link': pdf_link
            })
    
    # Para 'questionnaires' e 'manuals'
    else:
        for cabecalho in soup.find_all('h2'):
            titulo = cabecalho.get_text(strip=True)
            descricao = ""

            elemento_atual = cabecalho.find_next_sibling()
            while elemento_atual and elemento_atual.name != "h2":
                if elemento_atual.name == "p" and not descricao:
                    descricao = elemento_atual.get_text(strip=True)
                if elemento_atual.name == "table" and "doc-table" in elemento_atual.get("class", []):
                    tabela = elemento_atual
                    break
                elemento_atual = elemento_atual.find_next_sibling()

            if tabela:
                rows = tabela.find_all("tr")
                for row in rows[1:]:  # Ignora o cabeçalho
                    celulas = row.find_all('td')

                    nome_modulo = celulas[0].get_text(strip=True) if len(celulas) > 0 else ""
                    inline_desc = ""
                    div_tag = celulas[0].find("div") if len(celulas) > 0 else None
                    if div_tag:
                        inline_desc = div_tag.get_text(strip=True)

                    documento = ""
                    link = ""

                    # Verifica todas as células por possíveis links
                    for celula in celulas:
                        a_tag = celula.find("a")
                        if a_tag and a_tag.has_attr("href"):
                            link = a_tag["href"]
                            if link.startswith('/'):
                                link = "https://wwwn.cdc.gov" + link
                            documento = a_tag.get_text(strip=True)
                            break  # Para na primeira ocorrência de link válido

                    dados.append({
                        'Titulo': titulo,
                        'Descricao': descricao,
                        'Nome_Modulo': nome_modulo,
                        'Inline Description': inline_desc,
                        'Documento': documento,
                        'Link': link
                    })

    # Salvar os dados no CSV
    df = pd.DataFrame(dados)
    df.to_csv(nome_arquivo, index=False, encoding='utf-8')
    print(f"Extração concluída! Arquivo salvo como '{nome_arquivo}'")
