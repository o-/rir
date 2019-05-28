#!/usr/bin/env ruby

require 'json'
require 'date'

REPOS = {
  '10979413': { # rir_mirror
    repos: [
      560429,  # /
      560812   # /benchmarks
    ],
    keep: [
      `git rev-parse HEAD`.chomp,                   # current version
      `git rev-parse HEAD~1`.chomp,                 # prev version
      'master',
      # referenced in paper
      '73655471164f3418448daaee7bbacfa1ed4e8d50',
    ]},
  '12325205': {# rir experiments
    repos: [
      562769,  # scope_resolution
      576865,  # envs_created
    ],
    keep: [
      '73655471164f3418448daaee7bbacfa1ed4e8d50-908e16a9dd7ea5cddb8118c2c8c4a9f47a0b6479',
    ]},
}

TOKEN = ARGF.read

def curl(what)
  JSON.parse(`curl -s --header "PRIVATE-TOKEN: #{TOKEN}" #{what}`)
end

def fetch(project, repo, what)
  curl("https://gitlab.com/api/v4/projects/#{project}/registry/repositories/#{repo}/#{what}")
end

def delete(project, repo, what)
  curl("--request DELETE https://gitlab.com/api/v4/projects/#{project}/registry/repositories/#{repo}/#{what}")
end

MAX_AGE_DAYS=0.6

REPOS.each do |project, repos|
  repos[:repos].each do |repo|
    puts "== #{project} == #{repo} =="
    res = fetch(project, repo, "tags")
    res.each do |tag|
      if repos[:keep].include? tag['name']
        puts "keeping #{tag['name']} (whitelisted)"
      else
        info = fetch(project, repo, "tags/#{tag['name']}")
        t = DateTime.parse(info["created_at"])
        age = DateTime.now - t

        if age > MAX_AGE_DAYS
          puts "delete #{tag['name']} which is #{age.to_f}d old"
          puts delete(project, repo, "tags/#{tag['name']}")
        else
          puts "keeping #{tag['name']} (less than #{MAX_AGE_DAYS}d old)"
        end
      end
    end
  end
end