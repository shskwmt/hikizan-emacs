import chromadb
import os
from sentence_transformers import SentenceTransformer

class LongTermMemory:
    """
    A class to manage the agent's long-term memory using ChromaDB.
    """
    def __init__(self, db_path=os.path.join(os.path.expanduser("~"), "emacs_agent_chroma_db"), collection_name="emacs_agent_memory"):
        """
        Initializes the long-term memory system.

        Args:
            db_path (str): The path to the directory where the database will be stored.
            collection_name (str): The name of the collection within the database.
        """
        self.client = chromadb.PersistentClient(path=db_path)
        self.embedding_model = SentenceTransformer('all-MiniLM-L6-v2')
        self.collection = self.client.get_or_create_collection(
            name=collection_name,
            metadata={"hnsw:space": "cosine"} # Using cosine similarity
        )
        print(f"Long-term memory initialized. Collection '{collection_name}' has {self.collection.count()} items.")

    def add_memory(self, text_to_remember: str, metadata: dict = None):
        """
        Adds a new memory to the database.

        Args:
            text_to_remember (str): The text content of the memory.
            metadata (dict, optional): Optional metadata to store with the memory.
        """
        if not text_to_remember.strip():
            print("Skipping empty memory.")
            return

        embedding = self.embedding_model.encode([text_to_remember])[0].tolist()
        # ChromaDB requires a unique ID for each entry. We can use a hash of the text.
        doc_id = str(hash(text_to_remember))

        # Avoid adding duplicates
        if self.collection.get(ids=[doc_id])['ids']:
            print(f"Memory with ID {doc_id} already exists. Skipping.")
            return

        self.collection.add(
            ids=[doc_id],
            embeddings=[embedding],
            documents=[text_to_remember],
            metadatas=[metadata or {}]
        )
        print(f"Added new memory: '{text_to_remember[:50]}...'")

    def retrieve_relevant_memory(self, query_text: str, n_results: int = 3) -> list[str]:
        """
        Retrieves the most relevant memories based on a query.

        Args:
            query_text (str): The text to search for relevant memories.
            n_results (int): The maximum number of relevant memories to return.

        Returns:
            list[str]: A list of the most relevant memory texts.
        """
        if self.collection.count() == 0:
            return []

        query_embedding = self.embedding_model.encode([query_text])[0].tolist()
        results = self.collection.query(
            query_embeddings=[query_embedding],
            n_results=min(n_results, self.collection.count()) # Ensure n_results is not > items in collection
        )
        return results['documents'][0]
