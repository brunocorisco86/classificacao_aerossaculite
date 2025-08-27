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
    
    # Adicione mais análises e visualizações aqui
    # Exemplo:
    # st.subheader("Estatísticas Descritivas")
    # st.write(df.describe())
else:
    st.info("Aguardando o carregamento de um arquivo CSV.")
