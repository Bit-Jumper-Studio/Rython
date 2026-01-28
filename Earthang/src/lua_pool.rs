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
use mlua::Lua;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};

#[derive(Clone)]
pub struct LuaPool {
    pool: Arc<Mutex<VecDeque<Lua>>>,
    max_size: usize,
}

impl LuaPool {
    pub fn new(initial_size: usize, max_size: usize) -> Self {
        let mut pool = VecDeque::with_capacity(initial_size);
        
        for _ in 0..initial_size {
            let lua = Lua::new();
            pool.push_back(lua);
        }

        LuaPool {
            pool: Arc::new(Mutex::new(pool)),
            max_size,
        }
    }

    pub fn get_instance(&self) -> Option<Lua> {
        let mut pool = self.pool.lock().unwrap();
        if let Some(lua) = pool.pop_front() {
            Some(lua)
        } else {
            // Try to create a new instance if pool is empty
            Some(Lua::new())
        }
    }

    pub fn return_instance(&self, lua: Lua) {
        let mut pool = self.pool.lock().unwrap();
        if pool.len() < self.max_size {
            pool.push_back(lua);
        }
    }

    pub fn current_size(&self) -> usize {
        let pool = self.pool.lock().unwrap();
        pool.len()
    }

    pub fn max_size(&self) -> usize {
        self.max_size
    }
}

impl Default for LuaPool {
    fn default() -> Self {
        Self::new(10, 100)
    }
}