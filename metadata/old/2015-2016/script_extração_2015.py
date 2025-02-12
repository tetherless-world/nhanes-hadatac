import requests
from bs4 import BeautifulSoup
import csv
import pandas as pd

# Lista de URLs para processar
urls = [
    "https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/questionnaires.aspx?BeginYear=2015",
    "https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/labmethods.aspx?BeginYear=2015",
    "https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/manuals.aspx?BeginYear=2015"
]

# Itera sobre cada URL
for url in urls:
    # Extrai o ano e a categoria a partir da URL
    ano = url.split("BeginYear=")[-1]
    categoria = url.split("continuousnhanes/")[-1].split(".aspx")[0]
    
    print(f"\nProcessando categoria: {categoria} - Ano: {ano}")
    
    # Se a categoria for "labmethods", utiliza a extração via tabela com DictWriter do csv
    if categoria.lower() == "labmethods":
        nome_arquivo = f'{categoria}_{ano}.csv'
        response = requests.get(url)
        response.encoding = 'utf-8'
        soup = BeautifulSoup(response.text, 'html.parser')
        
        # Procura pela tabela que contém os dados
        table = soup.find('table')
        if not table:
            print("Não foi possível encontrar a tabela com os dados na página:", url)
            continue
        
        rows = table.find_all('tr')
        
        # Abre (ou cria) o arquivo CSV para escrita
        with open(nome_arquivo, 'w', newline='', encoding='utf-8') as csvfile:
            fieldnames = ['Lab Method Name', 'Documentation', 'Release Date', 'PDF Link']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            
            # Pula o cabeçalho da tabela e processa cada linha
            for row in rows[1:]:
                cols = row.find_all('td')
                if len(cols) >= 3:
                    lab_method_name = cols[0].get_text(strip=True)
                    
                    # Procura a tag <a> na coluna "Documentation"
                    a_tag = cols[1].find('a')
                    if a_tag and a_tag.has_attr('href'):
                        link_pdf = a_tag['href']
                        if link_pdf.startswith('/'):
                            link_pdf = "https://wwwn.cdc.gov" + link_pdf
                        documentation = a_tag.get_text(strip=True)
                    else:
                        documentation = cols[1].get_text(strip=True)
                        link_pdf = ""
                    
                    release_date = cols[2].get_text(strip=True)
                    
                    writer.writerow({
                        'Lab Method Name': lab_method_name,
                        'Documentation': documentation,
                        'Release Date': release_date,
                        'PDF Link': link_pdf
                    })
        
        print(f"Extração concluída! Arquivo salvo como '{nome_arquivo}'")
    
    # Para as demais categorias (questionnaires e manuals)
    else:
        nome_arquivo = f'{categoria}_{ano}.csv'
        response = requests.get(url)
        soup = BeautifulSoup(response.content, 'html.parser')
        dados = []
        
        # Procura por cada módulo identificado por <h2>
        for cabecalho in soup.find_all('h2'):
            titulo = cabecalho.get_text(strip=True)
            
            # Coleta os elementos que pertencem a este módulo
            conteudo_modulo = []
            for sibling in cabecalho.find_next_siblings():
                if sibling.name == 'h2':
                    break
                conteudo_modulo.append(sibling)
            
            # Busca a primeira tag <p> do módulo para usar como descrição
            descricao = ""
            for elem in conteudo_modulo:
                if elem.name == 'p':
                    descricao = elem.get_text(strip=True)
                    break
            
            # Procura por uma tabela dentro deste módulo
            tabela = None
            for elemento in conteudo_modulo:
                if elemento.name == 'table':
                    tabela = elemento
                    break
            
            # Se houver uma tabela com linhas de dados, processa cada linha (ignorando o cabeçalho)
            if tabela:
                linhas = tabela.find_all('tr')
                if len(linhas) > 1:
                    for linha in linhas[1:]:
                        celulas = linha.find_all('td')
                        if len(celulas) >= 2:
                            nome_modulo = celulas[0].get_text(strip=True)
                            documento = celulas[1].get_text(strip=True)
                            
                            # Se houver um link na célula, extrai-o
                            a_tag = celulas[1].find('a')
                            link = a_tag['href'] if a_tag and a_tag.has_attr('href') else ""
                            if link.startswith('/'):
                                link = "https://wwwn.cdc.gov" + link
                            
                            dados.append({
                                'Titulo': titulo,
                                'Descricao': descricao,
                                'Nome_Modulo': nome_modulo,
                                'Documento': documento,
                                'Link': link
                            })
                        else:
                            dados.append({
                                'Titulo': titulo,
                                'Descricao': descricao,
                                'Nome_Modulo': "",
                                'Documento': "",
                                'Link': ""
                            })
                else:
                    dados.append({
                        'Titulo': titulo,
                        'Descricao': descricao,
                        'Nome_Modulo': "",
                        'Documento': "",
                        'Link': ""
                    })
            else:
                dados.append({
                    'Titulo': titulo,
                    'Descricao': descricao,
                    'Nome_Modulo': "",
                    'Documento': "",
                    'Link': ""
                })
        
        # Cria um DataFrame e salva os dados extraídos em CSV
        df = pd.DataFrame(dados)
        df.to_csv(nome_arquivo, index=False, encoding='utf-8')
        print(f"Extração concluída! Arquivo salvo como '{nome_arquivo}'")
