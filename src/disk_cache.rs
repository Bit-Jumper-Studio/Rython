/*
    Copyright (C) 2026 Emanuel

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
*/
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::time::{UNIX_EPOCH};
use std::hash::{Hash, Hasher};
use std::collections::hash_map::DefaultHasher;

const GREEN: &str = "\x1b[32m";
const RESET: &str = "\x1b[0m";

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CachedScript {
    pub hash: String,
    pub original_path: String,
    pub bytecode: Vec<u8>,
    pub functions: HashMap<String, String>,
    pub timestamp: u64,
    pub dependencies: Vec<String>,
}

pub struct DiskCache {
    cache_dir: String,
}

pub struct CacheStats {
    pub total_files: usize,
    pub total_size: u64,
    pub cache_dir: String,
    pub files: Vec<CacheFileInfo>,
}

pub struct CacheFileInfo {
    pub path: String,
    pub size: u64,
    pub modified: std::time::SystemTime,
}

impl DiskCache {
    pub fn new() -> Self {
        let cache_dir = "./.cache".to_string();
        
        // initialize the cache directory to ensure persistent storage for compiled scripts between runs
        if let Err(e) = std::fs::create_dir_all(&cache_dir) {
            eprintln!("Warning: Could not create cache directory: {}", e);
        }
        
        DiskCache { cache_dir }
    }

    pub fn calculate_file_hash(path: &str) -> Result<String, String> {
        let metadata = std::fs::metadata(path)
            .map_err(|e| format!("Could not read file metadata: {}", e))?;
        
        let mut hasher = DefaultHasher::new();
        path.hash(&mut hasher);
        metadata.len().hash(&mut hasher);
        if let Ok(modified) = metadata.modified() {
            if let Ok(duration) = modified.duration_since(UNIX_EPOCH) {
                duration.as_secs().hash(&mut hasher);
            }
        }
        
        Ok(hasher.finish().to_string())
    }

    pub fn get_cached_script(&self, original_path: &str) -> Option<CachedScript> {
        let hash = match Self::calculate_file_hash(original_path) {
            Ok(hash) => hash,
            Err(_) => return None,
        };
        
        let cache_path = Path::new(&self.cache_dir).join(format!("{}.json", hash));
        
        if !cache_path.exists() {
            return None;
        }
        
        match fs::read_to_string(&cache_path) {
            Ok(content) => {
                match serde_json::from_str(&content) {
                    Ok(script) => {
                        println!("{}Cache hit for {}{}", GREEN, original_path, RESET);
                        Some(script)
                    }
                    Err(e) => {
                        eprintln!("Cache deserialization error: {}, removing corrupted file", e);
                        let _ = fs::remove_file(&cache_path);
                        None
                    }
                }
            }
            Err(e) => {
                eprintln!("Cache read error: {}", e);
                None
            }
        }
    }

    pub fn cache_script(&self, script: CachedScript) -> Result<(), String> {
        let cache_path = Path::new(&self.cache_dir).join(format!("{}.json", script.hash));
        
        let serialized = serde_json::to_string_pretty(&script)
            .map_err(|e| format!("Serialization error: {}", e))?;
            
        fs::write(&cache_path, serialized)
            .map_err(|e| format!("Could not write cache file: {}", e))?;
            
        Ok(())
    }

    pub fn clear_cache(&self) -> Result<(), String> {
        if let Ok(entries) = fs::read_dir(&self.cache_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() && path.extension().map_or(false, |ext| ext == "json") {
                    if let Err(e) = fs::remove_file(&path) {
                        eprintln!("Warning: Could not remove cache file {}: {}", path.display(), e);
                    }
                }
            }
        }
        
        println!("{}Cache cleared successfully{}", GREEN, RESET);
        Ok(())
    }

    pub fn cache_stats(&self) -> Result<(usize, usize), String> {
        let mut script_count = 0;
        let mut total_size = 0;
        
        if let Ok(entries) = fs::read_dir(&self.cache_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() && path.extension().map_or(false, |ext| ext == "json") {
                    script_count += 1;
                    total_size += entry.metadata().map(|m| m.len()).unwrap_or(0) as usize;
                }
            }
        }
        
        Ok((script_count, total_size))
    }

     // allowing cache operations without breaking existing integrations
     pub fn clear(&self) -> Result<(), String> {
        if std::path::Path::new(&self.cache_dir).exists() {
            std::fs::remove_dir_all(&self.cache_dir)
                .map_err(|e| e.to_string())?;
            std::fs::create_dir_all(&self.cache_dir)
                .map_err(|e| e.to_string())?;
        }
        Ok(())
    }

    pub fn get_stats(&self) -> Result<CacheStats, String> {
        use std::fs;
        use std::time::SystemTime;
        
        let mut total_files = 0;
        let mut total_size = 0;
        let mut files = Vec::new();
        
        if let Ok(entries) = fs::read_dir(&self.cache_dir) {
            for entry in entries.flatten() {
                if let Ok(metadata) = entry.metadata() {
                    if metadata.is_file() {
                        total_files += 1;
                        total_size += metadata.len();
                        
                        files.push(CacheFileInfo {
                            path: entry.path().to_string_lossy().to_string(),
                            size: metadata.len(),
                            modified: metadata.modified().unwrap_or_else(|_| SystemTime::now()),
                        });
                    }
                }
            }
        }
        
        Ok(CacheStats {
            total_files,
            total_size,
            cache_dir: self.cache_dir.clone(),
            files,
        })
    }
}

impl Default for DiskCache {
    fn default() -> Self {
        Self::new()
    }
}