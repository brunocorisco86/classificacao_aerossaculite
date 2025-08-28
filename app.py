import streamlit as st
import pandas as pd

st.set_page_config(layout="wide")

st.title("Análise de Dados de Aerossaculite")

st.markdown("Esta aplicação realiza a análise de dados referentes a aerossaculite.")

# Carregar dados
uploaded_file = st.sidebar.file_uploader("Carregue seu arquivo CSV", type=["csv"])

if uploaded_file is not None:
    df = pd.read_csv(uploaded_file)
    st.sidebar.success("Arquivo carregado com sucesso!")
    
    st.subheader("Visualização dos Dados")
    st.dataframe(df.head())
    
    # --- Análise de Incidência Mensal ---
    st.subheader("Análise de Incidência por Mês")

    # Verificar se as colunas necessárias existem
    required_columns = ['abate_dt', 'logaero_float']
    if all(col in df.columns for col in required_columns):
        # Converter a coluna de data para o formato datetime, tratando possíveis erros
        df['abate_dt'] = pd.to_datetime(df['abate_dt'], errors='coerce')

        # Criar a coluna 'mes'
        df['mes'] = df['abate_dt'].dt.month

        # Calcular a média de logaero por mês e ordenar
        media_logaero_mes = df.groupby('mes')['logaero_float'].mean().sort_values(ascending=False)

        st.markdown("Meses com maior incidência média de aerossaculite (`logaero_float`):")
        st.dataframe(media_logaero_mes)

        st.markdown("Visualização da incidência média por mês:")
        st.bar_chart(media_logaero_mes)

    else:
        st.warning(f"Para a análise mensal, o arquivo CSV precisa conter as colunas: {', '.join(required_columns)}")
else:
    st.info("Aguardando o carregamento de um arquivo CSV.")
